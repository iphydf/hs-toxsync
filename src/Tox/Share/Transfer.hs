{-# LANGUAGE StrictData    #-}
{-# LANGUAGE TupleSections #-}
-- | FT1 file transfers with a group's peers, as ngcft1 runs them. The caller
-- passes in the time, the packets that arrive and its decisions, and sends
-- the packets that come out. @p@ names a peer.
--
-- Unlike ngcft1, a receiving transfer keeps at most 'maxAhead' packets past
-- a gap and gives up when the sender goes quiet, so a peer can't make us
-- hold more than that.
module Tox.Share.Transfer
  ( Transfers
  , Output (..)
  , new
  , start
  , accept
  , decline
  , receive
  , tick
  , leave
  , held
  , active
  , sending
  ) where

import qualified Data.ByteString      as BS
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromMaybe)
import           Data.Word            (Word16, Word64)
import           System.Random        (StdGen, uniformR)

import qualified Tox.Share.Congestion as Congestion
import           Tox.Share.Congestion (Congestion, Seconds)
import           Tox.Share.Packet     (FileKind, Packet (..), TransferId)

data Transfers p = Transfers
  { peers    :: Map.Map p Peer
  , maxData  :: Int
    -- ^ The most data our lossy packets carry.
  , gen      :: StdGen
  , lastTick :: Seconds
  }

data Peer = Peer
  { sends      :: Map.Map TransferId Sending
  , receives   :: Map.Map TransferId Receiving
  , offers     :: Map.Map TransferId (FileKind, BS.ByteString, Word64)
  , nextSend   :: TransferId
  , nextTurn   :: TransferId
  , control    :: Maybe Congestion
  , packetData :: Int
  }

data SendState = InitSent | SendingData | Finishing
  deriving (Eq)

data Sending = Sending
  { sendKind   :: FileKind
  , sendId     :: BS.ByteString
  , sendState  :: SendState
  , inits      :: Int
  , sendActive :: Seconds
  , sendSize   :: Word64
  , sentBytes  :: Word64
  , unacked    :: Map.Map Word16 BS.ByteString
  , nextSeq    :: Word16
  , source     :: Word64 -> Int -> BS.ByteString
  }

data Receiving = Receiving
  { recvSize   :: Word64
  , gotBytes   :: Word64
  , recvActive :: Seconds
  , done       :: Bool
  , pending    :: Map.Map Word16 BS.ByteString
  , expected   :: Word16
  , recentSeqs :: [Word16]
  }

data Output p
  = Send p Packet
  | Offered p TransferId FileKind BS.ByteString Word64
    -- ^ The peer wants to send us this file, of this size. 'accept' takes
    -- it.
  | Got p TransferId Word64 BS.ByteString
    -- ^ The next bytes of a file we receive, at this offset.
  | Received p TransferId Bool
    -- ^ A receiving transfer ended, and whether we got all of it.
  | Sent p TransferId Bool
    -- ^ A sending transfer ended, and whether the peer got all of it.
  deriving (Eq, Show)

type Step p = (Transfers p, [Output p])

-- | INIT goes again after this many round trips without an answer.
initRetry :: Double
initRetry = 16

maxInits :: Int
maxInits = 8

-- | Seconds of quiet before a transfer is given up, for each transfer
-- running the same way with the peer, so that they don't starve each other.
giveUpAfter :: Seconds
giveUpAfter = 30

-- | How long a complete receiving transfer stays, so that late packets are
-- still acked.
lingerFor :: Seconds
lingerFor = 2

-- | Acks repeat the last this many packets.
ackCount :: Int
ackCount = 3

-- | Packets kept past a gap in a receiving transfer.
maxAhead :: Int
maxAhead = 1024

-- | With the most data our Tox's lossy group packets carry (its largest
-- custom lossy packet less 4), and a random generator.
new :: Seconds -> Int -> StdGen -> Transfers p
new now size g = Transfers Map.empty size g now

emptyPeer :: Peer
emptyPeer = Peer Map.empty Map.empty Map.empty 0 0 Nothing 0

peerOf :: Ord p => p -> Transfers p -> Peer
peerOf p = fromMaybe emptyPeer . Map.lookup p . peers

withPeer :: Ord p => p -> Peer -> Transfers p -> Transfers p
withPeer p peer t = t { peers = Map.insert p peer (peers t) }

-- | Offers the peer a file of this kind, ID and size, whose bytes come from
-- the function, which must give exactly as many as asked for at an offset.
-- 'Nothing' for an empty file, or when all 256 transfer IDs are in use.
start :: Ord p => Seconds -> p -> FileKind -> BS.ByteString -> Word64 -> (Word64 -> Int -> BS.ByteString) -> Transfers p -> Maybe (TransferId, Step p)
start now p kind fileId size bytes t
  | size == 0 = Nothing
  | otherwise = case filter (`Map.notMember` sends peer) (take 256 [nextSend peer ..] <> [minBound ..]) of
    [] -> Nothing
    i : _ -> Just (i, (withPeer p peer { sends = Map.insert i transfer (sends peer), nextSend = nextSend peer + 1 } t, [Send p (Init kind size i fileId)]))
  where
    peer = peerOf p t
    transfer = Sending kind fileId InitSent 1 now size 0 Map.empty 0 bytes

-- | Takes a file 'Offered' to us.
accept :: Ord p => Seconds -> p -> TransferId -> Transfers p -> Step p
accept now p i t = case Map.lookup i (offers peer) of
  Nothing -> (t, [])
  Just (_, _, size) ->
    ( withPeer p peer { offers = Map.delete i (offers peer), receives = Map.insert i (Receiving size 0 now False Map.empty 0 []) (receives peer) } t
    , [Send p (InitAck i (fromIntegral (maxData t)) 0)] )
  where peer = peerOf p t

-- | Forgets a file 'Offered' to us that we don't take.
decline :: Ord p => p -> TransferId -> Transfers p -> Transfers p
decline p i t = withPeer p peer { offers = Map.delete i (offers peer) } t
  where peer = peerOf p t

-- | A packet from the peer. Packets that aren't about transfers do nothing.
receive :: Ord p => Seconds -> p -> Packet -> Transfers p -> Step p
receive now p packet t = case packet of
  Init kind size i fileId    -> offered kind size i fileId
  Init2 kind size i _ fileId -> offered kind size i fileId
  InitAck i size _           -> initAcked i (fromIntegral size)
  Data i sequ bytes          -> got i sequ bytes
  DataAck i sequs            -> dataAcked i sequs
  _                          -> (t, [])
  where
    peer = peerOf p t

    offered kind size i fileId =
      (withPeer p peer { offers = Map.insert i (kind, fileId, size) (offers peer) } t, [Offered p i kind fileId size])

    initAcked i size = case Map.lookup i (sends peer) of
      Just s | sendState s == InitSent && size >= 16 ->
        let (extra, g) = uniformR (0, 200) (gen t)
            -- Random within 1020 to 1220, as ngcft1 picks it.
            limit = min (min size (maxData t)) (1020 + extra)
            peer' = case control peer of
              Just _  -> peer
              Nothing -> peer { control = Just (Congestion.new now limit), packetData = limit }
        in (withPeer p peer' { sends = Map.insert i s { sendState = SendingData, sendActive = now } (sends peer) } t { gen = g }, [])
      _ -> (t, [])

    got i sequ bytes = case Map.lookup i (receives peer) of
      Just r | not (BS.null bytes) ->
        let ahead = fromIntegral (sequ - expected r)
            stored
              | done r || ahead >= maxAhead = r
              | otherwise = r { pending = Map.insert sequ bytes (pending r) }
            (r', outs) = deliver p i stored { recvActive = now, recentSeqs = take ackCount (sequ : recentSeqs r) }
            finished = not (done r') && gotBytes r' >= recvSize r'
            ack = Send p (DataAck i (recentSeqs r'))
        in ( withPeer p peer { receives = Map.insert i r' { done = done r' || finished } (receives peer) } t
           , outs <> [ack] <> [Received p i True | finished] )
      _ -> (t, [])

    dataAcked i sequs = case Map.lookup i (sends peer) of
      Just s | sendState s /= InitSent ->
        let s' = s { sendActive = now, unacked = foldl' (flip Map.delete) (unacked s) sequs }
            control' = Congestion.acked now (map (i,) sequs) <$> control peer
            complete = sentBytes s' >= sendSize s' && Map.null (unacked s')
            peer' = peer { control = control', sends = if complete then Map.delete i (sends peer) else Map.insert i s' (sends peer) }
        in (withPeer p peer' t, [Sent p i True | complete])
      _ -> (t, [])

-- | Passes on the bytes that now follow without a gap, up to the file's
-- size.
deliver :: p -> TransferId -> Receiving -> (Receiving, [Output p])
deliver p i r = case Map.lookup (expected r) (pending r) of
  Nothing -> (r, [])
  Just bytes ->
    let (r', outs) = deliver p i r { pending = Map.delete (expected r) (pending r), expected = expected r + 1, gotBytes = gotBytes r + fromIntegral (BS.length bytes) }
    in (r', Got p i (gotBytes r) (BS.take (fromIntegral (recvSize r - gotBytes r)) bytes) : outs)

-- | Resends what timed out, sends new data as far as congestion control
-- allows, and gives up on transfers that went quiet. Call it often while
-- transfers run (ngcft1 does every 5 ms).
tick :: Ord p => Seconds -> Transfers p -> Step p
tick now t0 = foldl' step (t0 { lastTick = now }, []) (Map.keys (peers t0))
  where
    dt = now - lastTick t0
    step (t, outs) p = let (t', outs') = tickPeer now dt p t in (t', outs <> outs')

tickPeer :: Ord p => Seconds -> Seconds -> p -> Transfers p -> Step p
tickPeer now dt p t = (withPeer p peer2 t, recvOuts <> sendOuts)
  where
    (peer1, recvOuts) = tickReceives now p (peerOf p t)
    (peer2, sendOuts) = maybe (retryInits now 2 p peer1) (tickSends now dt p peer1) (control peer1)

tickReceives :: Seconds -> p -> Peer -> (Peer, [Output p])
tickReceives now p peer = (peer { receives = Map.filter keep (receives peer) }, [Received p i False | (i, r) <- Map.toList (receives peer), quiet r])
  where
    running = Map.size (receives peer)
    quiet r = not (done r) && now - recvActive r >= giveUpAfter * fromIntegral running
    keep r
      | done r = now - recvActive r < lingerFor
      | otherwise = not (quiet r)

-- | Sends INIT again where the peer hasn't answered for a while, or gives
-- up. Before the peer's first INIT_ACK there's no round trip to go by, so
-- the longest is assumed.
retryInits :: Seconds -> Seconds -> p -> Peer -> (Peer, [Output p])
retryInits now roundTrip p peer = foldl' retry (peer, []) (Map.toList (sends peer))
  where
    retry (pr, outs) (i, s)
      | sendState s /= InitSent || now - sendActive s < initRetry * min roundTrip 2 = (pr, outs)
      | inits s >= maxInits = (pr { sends = Map.delete i (sends pr) }, outs <> [Sent p i False])
      | otherwise = ( pr { sends = Map.insert i s { inits = inits s + 1, sendActive = now } (sends pr) }
                    , outs <> [Send p (Init (sendKind s) (sendSize s) i (sendId s))] )

-- | Gives up on quiet transfers, resends timed-out data while there's room,
-- then sends new data with the rest, taking the transfers in turn.
tickSends :: Seconds -> Seconds -> p -> Peer -> Congestion -> (Peer, [Output p])
tickSends now dt p peer0 c0 = (peer3 { control = Just c5 }, initOuts <> quietOuts <> resent <> fresh)
  where
    (late, c1) = Congestion.timeouts now c0
    (room0, c2) = Congestion.canSend now dt c1
    (peer1, initOuts) = retryInits now (Congestion.rtt c2) p peer0
    running = Map.size (sends peer1)
    quiet s = sendState s /= InitSent && now - sendActive s >= giveUpAfter * fromIntegral running
    (gone, kept) = Map.partition quiet (sends peer1)
    quietOuts = [Sent p i False | i <- Map.keys gone]
    c3 = foldl' (\c key -> snd (Congestion.lost now True key c)) c2 [(i, sequ) | (i, s) <- Map.toList gone, sequ <- Map.keys (unacked s)]
    (resent, room1, c4) = foldl' again ([], room0, c3) [(i, sequ, bytes) | (i, s) <- Map.toList kept, (sequ, bytes) <- Map.toList (unacked s), (i, sequ) `elem` late]
    again (outs, room, c) (i, sequ, bytes)
      | room < BS.length bytes = (outs, room, c)
      | otherwise =
          let (found, c') = Congestion.lost now False (i, sequ) c
          in ( outs <> [Send p (Data i sequ bytes)]
             , room - BS.length bytes
             , if found then c' else Congestion.sent now (i, sequ) (BS.length bytes) c' )
    -- From where the last tick ran out of room, so no transfer starves.
    order = let (before, after) = Map.spanAntitone (< nextTurn peer1) kept in Map.keys after <> Map.keys before
    (peer2, fresh, _, c5, starved) = foldl' sendOne (peer1 { sends = kept }, [], room1, c4, Nothing) order
    peer3 = maybe peer2 (\i -> peer2 { nextTurn = i }) starved
    sendOne (pr, outs, room, c, firstStarved) i
      | room <= 0 = (pr, outs, room, c, Just (fromMaybe i firstStarved))
      | otherwise = case Map.lookup i (sends pr) of
          Just s | sendState s == SendingData ->
            let (s', outs', room', c') = fill s [] room c
            in (pr { sends = Map.insert i s' (sends pr) }, outs <> outs', room', c', firstStarved)
          _ -> (pr, outs, room, c, firstStarved)
      where
        fill s acc r cc
          | sentBytes s >= sendSize s = (s { sendState = Finishing }, acc, r, cc)
          | r <= 0 = (s, acc, r, cc)
          | otherwise =
              let size = minimum [packetData pr, r, fromIntegral (sendSize s - sentBytes s)]
                  bytes = source s (sentBytes s) size
                  sequ = nextSeq s
              in fill s { unacked = Map.insert sequ bytes (unacked s), nextSeq = sequ + 1, sentBytes = sentBytes s + fromIntegral size }
                      (acc <> [Send p (Data i sequ bytes)]) (r - size) (Congestion.sent now (i, sequ) size cc)

-- | The peer left the group: every transfer with it ends.
leave :: Ord p => p -> Transfers p -> Step p
leave p t = (t { peers = Map.delete p (peers t) }, [Sent p i False | i <- Map.keys (sends peer)] <> [Received p i False | (i, r) <- Map.toList (receives peer), not (done r)])
  where peer = peerOf p t

-- | Bytes held for receiving transfers past a gap, to watch that peers can't
-- make it grow.
held :: Transfers p -> Int
held = sum . map (sum . map (sum . fmap BS.length . pending) . Map.elems . receives) . Map.elems . peers

-- | Files we send the peer now, or offer it.
sending :: Ord p => p -> Transfers p -> Int
sending p = Map.size . sends . peerOf p

-- | Whether any transfer runs, or waits to be taken.
active :: Transfers p -> Bool
active = any (\peer -> not (Map.null (sends peer) && Map.null (receives peer) && Map.null (offers peer))) . peers
