{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
-- | Sharing files with a group's peers, compatible with Green-Sky's clients:
-- "Tox.Share.Transfer" and "Tox.Share.Swarm" wired together. The caller
-- passes in the group's custom packets and the time, and sends what comes
-- out.
module Tox.Share
  ( Share
  , Output (..)
  , new
  , receive
  , tick
  , download
  , learn
  , fetch
  , pause
  , prioritise
  , readFrom
  , stats
  , busy
  , share
  , seed
  , sendChunk
  , requestHistory
  , answerHistory
  , joined
  , leave
  ) where

import qualified Data.ByteString    as BS
import qualified Data.Map.Strict    as Map
import           Data.Word          (Word32, Word64)
import           System.Random      (StdGen, mkStdGen, uniform)

import qualified Tox.Share.History  as History
import           Tox.Share.History  (Entry, Range)
import qualified Tox.Share.Info     as Info
import           Tox.Share.Info     (Info)
import qualified Tox.Share.Packet   as Packet
import           Tox.Share.Packet   (Channel, FileKind, Packet (..), TransferId,
                                     historyRange, sha1Chunk, sha1Info)
import qualified Tox.Share.Stats    as Stats
import           Tox.Share.Stats    (Stats, Tally)
import qualified Tox.Share.Swarm    as Swarm
import           Tox.Share.Swarm    (FileId, Priority, Swarm)
import qualified Tox.Share.Transfer as Transfer
import           Tox.Share.Transfer (Transfers)

type Seconds = Double

data Share p = Share
  { transfers :: Transfers p
  , swarm     :: Swarm p
  , askedLogs :: Map.Map p [Range]
  , logs      :: Map.Map (p, TransferId) [BS.ByteString]
  , outgoing  :: Map.Map (p, TransferId) FileId
    -- ^ The files of the chunks we send.
  , tally     :: Tally
  }

data Output p
  = Send p Packet
    -- ^ As a private custom packet, lossless unless 'Packet.lossless' says
    -- otherwise.
  | SendAll Packet
    -- ^ As a public lossless custom packet.
  | Shared p Word32 FileId
    -- ^ The peer shared a file in the group, by message ID. 'download'
    -- fetches it.
  | Learned FileId Info
  | Chunk FileId Int BS.ByteString
    -- ^ A checked chunk, to write at its index times the chunk size.
  | ChunkWanted p FileId Int
    -- ^ The peer asks for a chunk we have. 'sendChunk' sends it.
  | Finished FileId
  | HistoryWanted p Range
    -- ^ The peer asks for the group's messages in the range.
    -- 'answerHistory' sends them. Totato leaves out those from before the
    -- peer first joined.
  | History p [Entry]
    -- ^ The messages a peer sent for a range we asked for, newest first.
  deriving (Eq, Show)

type Step p = (Share p, [Output p])

-- | Takes the most data our Tox's lossy group packets carry (its largest
-- custom lossy packet, less 4).
new :: Seconds -> Int -> StdGen -> Share p
new now size g = Share (Transfer.new now size g1) (Swarm.empty (mkStdGen k)) Map.empty Map.empty Map.empty Stats.empty
  where (k, g1) = uniform g

-- | A custom packet from a peer. Packets that aren't ours do nothing.
receive :: Ord p => Seconds -> p -> Channel -> BS.ByteString -> Share p -> Step p
receive now p channel bytes s = case Packet.decode channel bytes of
  Nothing -> (s, [])
  Just packet ->
    let (sw, heardOuts) = Swarm.heard p packet (swarm s)
        (s2, transferOuts) = fromTransfer now (Transfer.receive now p packet (transfers s { swarm = sw })) s { swarm = sw }
        outs = map fromSwarm heardOuts <> transferOuts
        shared = [Shared p m fid | Message m kind fid <- [packet], kind == sha1Info]
          <> [HistoryWanted p r | Request kind ident <- [packet], kind == historyRange, Just r <- [History.decodeRange ident]]
    in case Swarm.wants packet (swarm s2) of
      Just (fid, Nothing) | Just inf <- Swarm.fileInfo fid (swarm s2) ->
        let (s3, served) = start now p sha1Info fid (Info.encode inf) s2 in (s3, outs <> served <> shared)
      Just (fid, Just n) | room p s2 -> (s2, outs <> [ChunkWanted p fid n] <> shared)
      _ -> (s2, outs <> shared)

-- | Chunks we send a peer at once. Clients ask a peer for 5 at a time, so
-- only stale requests go over this, and those are dropped.
maxSending :: Int
maxSending = 8

room :: Ord p => p -> Share p -> Bool
room p s = Transfer.sending p (transfers s) < maxSending

-- | Sends the peer the chunk it asked for with 'ChunkWanted'.
sendChunk :: Ord p => Seconds -> p -> FileId -> Int -> BS.ByteString -> Share p -> Step p
sendChunk now p fid n bytes s = case Swarm.fileInfo fid (swarm s) of
  Just inf
    | room p s, Just h <- Info.chunkHash inf n, BS.length bytes == Info.chunkLength inf n
    , Just (i, step) <- offer now p sha1Chunk h bytes s -> fromTransfer now step s { outgoing = Map.insert (p, i) fid (outgoing s) }
  _ -> (s, [])

offer :: Ord p => Seconds -> p -> FileKind -> BS.ByteString -> BS.ByteString -> Share p -> Maybe (TransferId, (Transfers p, [Transfer.Output p]))
offer now p kind name bytes = Transfer.start now p kind name (fromIntegral (BS.length bytes)) (\off len -> BS.take len (BS.drop (fromIntegral off) bytes)) . transfers

start :: Ord p => Seconds -> p -> FileKind -> BS.ByteString -> BS.ByteString -> Share p -> Step p
start now p kind name bytes s = maybe (s, []) (\(_, step) -> fromTransfer now step s) (offer now p kind name bytes s)

-- | Passes what the transfers report on to the swarm: offers, bytes and
-- ends.
fromTransfer :: Ord p => Seconds -> (Transfers p, [Transfer.Output p]) -> Share p -> Step p
fromTransfer now (t, outs) s0 = foldl one (s0 { transfers = t }, []) outs
  where
    one (s, acc) = \case
      Transfer.Send p packet -> (counted p packet s, acc <> [Send p packet])
      Transfer.Offered p i kind ident size
        | kind == historyRange, Just r <- History.decodeRange ident, r `elem` Map.findWithDefault [] p (askedLogs s), size <= maxLog ->
            let s' = s { askedLogs = Map.adjust (filter (/= r)) p (askedLogs s), logs = Map.insert (p, i) [] (logs s) }
                (s'', o) = fromTransfer now (Transfer.accept now p i (transfers s')) s'
            in (s'', acc <> o)
      Transfer.Offered p i kind fid size ->
        let (yes, sw) = Swarm.offered p i kind fid size (swarm s)
            s' = s { swarm = sw }
        in if yes then let (s'', o) = fromTransfer now (Transfer.accept now p i (transfers s')) s' in (s'', acc <> o) else (s' { transfers = Transfer.decline p i (transfers s') }, acc)
      Transfer.Got p i _ bytes
        | Map.member (p, i) (logs s) -> (s { logs = Map.adjust (bytes :) (p, i) (logs s) }, acc)
        | otherwise ->
            let counting = maybe id (\fid -> Stats.received now fid (BS.length bytes)) (Swarm.receiving p i (swarm s))
            in (s { swarm = Swarm.got p i bytes (swarm s), tally = counting (tally s) }, acc)
      Transfer.Received p i ok
        | Just parts <- Map.lookup (p, i) (logs s) ->
            ( s { logs = Map.delete (p, i) (logs s) }
            , acc <> [History p entries | ok, Just entries <- [History.decodeLog (BS.concat (reverse parts))]] )
        | otherwise -> let (sw, o) = Swarm.received p i ok (swarm s) in (s { swarm = sw }, acc <> map fromSwarm o)
      Transfer.Sent p i _ -> (s { outgoing = Map.delete (p, i) (outgoing s) }, acc)
    counted p packet s = case packet of
      Data i _ bytes | Just fid <- Map.lookup (p, i) (outgoing s) -> s { tally = Stats.sent now fid (BS.length bytes) (tally s) }
      _ -> s

fromSwarm :: Swarm.Output p -> Output p
fromSwarm = \case
  Swarm.Send p packet -> Send p packet
  Swarm.SendAll packet -> SendAll packet
  Swarm.Learned fid inf -> Learned fid inf
  Swarm.Chunk fid n bytes -> Chunk fid n bytes
  Swarm.Finished fid -> Finished fid

-- | Call it often while transfers run (ngcft1 does every 5 ms).
tick :: Ord p => Seconds -> Share p -> Step p
tick now s0 = (s2, outs1 <> map fromSwarm outs2)
  where
    (s1, outs1) = fromTransfer now (Transfer.tick now (transfers s0)) s0
    (sw, outs2) = Swarm.tick now (swarm s1)
    s2 = s1 { swarm = sw }

download :: Ord p => p -> FileId -> Share p -> Share p
download p fid s = s { swarm = Swarm.download p fid (swarm s) }

-- | Gets only the info of a file a peer shared, which 'Learned' reports.
learn :: Ord p => p -> FileId -> Share p -> Share p
learn p fid s = s { swarm = Swarm.learn p fid (swarm s) }

-- | Downloads the chunks of a file by its info, which 'Learned' gave,
-- maybe before a restart.
fetch :: Info -> Share p -> Share p
fetch inf s = s { swarm = Swarm.fetch inf (swarm s) }

pause :: FileId -> Share p -> Share p
pause fid s = s { swarm = Swarm.pause fid (swarm s) }

prioritise :: Priority -> FileId -> Share p -> Share p
prioritise prio fid s = s { swarm = Swarm.prioritise prio fid (swarm s) }

-- | The client reads the file from this byte on, as when playing it.
readFrom :: Word64 -> FileId -> Share p -> Share p
readFrom offset fid s = s { swarm = Swarm.readFrom offset fid (swarm s) }

stats :: Seconds -> FileId -> Share p -> Stats
stats now fid = Stats.stats now fid . tally

-- | Whether transfers run, which want 'tick' every few ms. Otherwise once a
-- second is enough.
busy :: Share p -> Bool
busy = Transfer.active . transfers

-- | Shares a file with the group, as a message with this ID. Its chunks go
-- out through 'ChunkWanted'.
share :: Word32 -> Info -> Share p -> Step p
share m inf s = (s { swarm = Swarm.share fid inf (swarm s) }, [SendAll (Message m sha1Info fid)])
  where fid = Info.hash inf

-- | Serves a file we have whole without announcing it, as after a
-- restart.
seed :: Info -> Share p -> Share p
seed inf s = s { swarm = Swarm.share (Info.hash inf) inf (swarm s) }

-- | Asks the peer for the last two days of the group's messages, as
-- totato does when a peer joins. The time is seconds since the Unix epoch.
requestHistory :: Ord p => Word64 -> p -> Share p -> Step p
requestHistory unixNow p s = (s { askedLogs = Map.insertWith (<>) p [r] (askedLogs s) }, [Send p (Request historyRange (History.encodeRange r))])
  where r = History.lastTwoDays unixNow

-- | Sends the peer the messages it asked for with 'HistoryWanted'.
answerHistory :: Ord p => Seconds -> p -> Range -> [Entry] -> Share p -> Step p
answerHistory now p r entries = start now p historyRange (History.encodeRange r) (History.encodeLog entries)

-- | Larger chatlogs are refused, as totato does.
maxLog :: Word64
maxLog = 64 * 1024 * 1024

-- | A peer joined the group.
joined :: Seconds -> Share p -> Step p
joined now s = (s { swarm = Swarm.joined now (swarm s) }, [])

-- | The peer left the group.
leave :: Ord p => p -> Share p -> Step p
leave p s = (s { transfers = t, swarm = Swarm.leave p (swarm s), askedLogs = Map.delete p (askedLogs s), logs = Map.filterWithKey (\(q, _) _ -> q /= p) (logs s), outgoing = Map.filterWithKey (\(q, _) _ -> q /= p) (outgoing s) }, [])
  where (t, _) = Transfer.leave p (transfers s)
