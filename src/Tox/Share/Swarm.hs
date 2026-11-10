{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE StrictData    #-}
{-# LANGUAGE TupleSections #-}
-- | Sharing and downloading files in a group's SHA1 swarm, as
-- sha1_ngcft1.cpp does: the file's info first, then its chunks from the
-- peers that have them, each checked against its SHA1. The caller passes on
-- what "Tox.Share.Transfer" reports and sends what comes out.
module Tox.Share.Swarm
  ( Swarm
  , FileId
  , Output (..)
  , Priority (..)
  , empty
  , download
  , learn
  , fetch
  , pause
  , prioritise
  , readFrom
  , share
  , wants
  , fileInfo
  , heard
  , offered
  , got
  , received
  , receiving
  , tick
  , joined
  , leave
  ) where

import           Control.Applicative ((<|>))
import           Data.Bits           (setBit, shiftR, testBit)
import qualified Data.ByteString     as BS
import qualified Data.Map.Strict     as Map
import           Data.Maybe          (isNothing, listToMaybe)
import qualified Data.Set            as Set
import           Data.Word           (Word64)
import           System.Random       (StdGen, uniform, uniformR)

import qualified Tox.Share.Info      as Info
import           Tox.Share.Info      (Info)
import           Tox.Share.Packet    (FileKind (..), Packet (..), TransferId,
                                      sha1Chunk, sha1Info)

type Seconds = Double

-- | The SHA1 of the file's info.
type FileId = BS.ByteString

data Swarm p = Swarm
  { files    :: Map.Map FileId (File p)
  , incoming :: Map.Map (p, TransferId) Incoming
  , gen      :: StdGen
  , turn     :: Maybe FileId
    -- ^ The file that picks chunks first next tick.
  }

data File p = File
  { info      :: Maybe Info
  , infoAsked :: Maybe Seconds
  , have      :: Set.Set Int
  , asked     :: Map.Map Int (p, Seconds)
  , holders   :: Map.Map p Holding
    -- ^ The peers taking part, with what they have, maybe nothing yet.
  , announce  :: Maybe (Seconds, Seconds)
    -- ^ While downloading, when to announce it next and the gap after.
  , wanted    :: Bool
    -- ^ Its chunks, not just its info.
  , priority  :: Priority
  , waited    :: Int
    -- ^ Ticks since it last picked chunks.
  , readHead  :: Word64
    -- ^ Where the client reads it, so that chunks from there on come first.
  , index     :: Map.Map BS.ByteString [Int]
    -- ^ Its chunks by SHA1, once we download or share it. Files we only
    -- learn about don't get one, as their info may be huge.
  }

blank :: File p
blank = File
  { info = Nothing, infoAsked = Nothing, have = Set.empty, asked = Map.empty, holders = Map.empty
  , announce = Nothing, wanted = False, priority = Normal, waited = 0, readHead = 0, index = Map.empty }

-- | Indexes its chunks if we download or share it.
indexed :: File p -> File p
indexed f = case info f of
  Just inf | Map.null (index f), wanted f || not (Set.null (have f)) ->
    f { index = Map.fromListWith (<>) [(h, [n]) | (n, h) <- zip [0 ..] (Info.chunks inf)] }
  _ -> f

data Priority = Highest | High | Normal | Low | Lowest
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Ticks a file waits between picking chunks, as in chunk_picker.cpp.
waits :: Priority -> Int
waits = \case
  Highest -> 0
  High    -> 1
  Normal  -> 2
  Low     -> 4
  Lowest  -> 8

data Holding = Everything | Some (Set.Set Int)

-- | What a transfer we took brings, so far.
data Incoming
  = IncomingInfo FileId [BS.ByteString]
  | IncomingChunk FileId [Int] [BS.ByteString]

data Output p
  = Send p Packet
  | SendAll Packet
  | Learned FileId Info
    -- ^ What the file is, before any of its chunks.
  | Chunk FileId Int BS.ByteString
    -- ^ A chunk, checked against its SHA1, to write at its index times the
    -- chunk size.
  | Finished FileId
  deriving (Eq, Show)

-- | Asks for a file's info again after this long without it.
infoRetry :: Seconds
infoRetry = 15

-- | A chunk asked for that hasn't come in this long can be asked for again.
chunkRetry :: Seconds
chunkRetry = 60

-- | The most chunk requests and transfers open with each peer.
maxRequests :: Int
maxRequests = 5

-- | The most files whose info we ask for at once.
maxInfoRequests :: Int
maxInfoRequests = 6

-- | We announce a file we download straight away, then after this long,
-- twice as long each time after that.
firstGap :: Seconds
firstGap = 60

-- | Chunks a BITSET covers at most.
bitsetChunks :: Int
bitsetChunks = 8 * 512

-- | Larger infos are refused.
maxInfo :: Word64
maxInfo = 100 * 1024 * 1024

-- | Picks chunks at random places with the generator, so that peers
-- downloading together ask for different chunks and can swap them.
empty :: StdGen -> Swarm p
empty g = Swarm Map.empty Map.empty g Nothing

download :: Ord p => p -> FileId -> Swarm p -> Swarm p
download p fid = want fid . learn p fid

-- | Gets only the info of the file a peer shared, to say what it is.
learn :: Ord p => p -> FileId -> Swarm p -> Swarm p
learn p fid s = s { files = Map.insertWith (\_ f -> f { holders = Map.insert p Everything (holders f) }) fid fresh (files s) }
  where fresh = blank { holders = Map.singleton p Everything }

-- | Downloads the chunks of a file we have the info of, from the peers that
-- answer our announcements, and from those we know have it.
fetch :: Info -> Swarm p -> Swarm p
fetch inf s = want fid s { files = Map.insertWith (\_ f -> f { info = info f <|> Just inf }) fid fresh (files s) }
  where
    fid = Info.hash inf
    fresh = blank { info = Just inf }

want :: FileId -> Swarm p -> Swarm p
want fid s = s { files = Map.adjust (\f -> indexed f { wanted = True, announce = announce f <|> Just (0, firstGap) }) fid (files s) }

-- | Stops asking for the file's chunks until 'fetch'.
pause :: FileId -> Swarm p -> Swarm p
pause fid s = s { files = Map.adjust (\f -> f { wanted = False, announce = Nothing }) fid (files s) }

prioritise :: Priority -> FileId -> Swarm p -> Swarm p
prioritise prio fid s = s { files = Map.adjust (\f -> f { priority = prio }) fid (files s) }

-- | The client reads the file from this byte on, so its chunks from there
-- are asked for first, half of the time.
readFrom :: Word64 -> FileId -> Swarm p -> Swarm p
readFrom offset fid s = s { files = Map.adjust (\f -> f { readHead = offset }) fid (files s) }

-- | Shares a file whose chunks we all have.
share :: FileId -> Info -> Swarm p -> Swarm p
share fid inf s = s { files = Map.insertWith keep fid whole (files s) }
  where
    whole = indexed blank { info = Just inf, have = Set.fromList [0 .. Info.chunkCount inf - 1] }
    keep _ f = f { info = Just inf, have = have whole, announce = Nothing, index = index whole }

-- | What a peer's REQUEST asks of us: a file's info ('Nothing'), or the
-- index of a chunk we have, to send with "Tox.Share.Transfer".
wants :: Packet -> Swarm p -> Maybe (FileId, Maybe Int)
wants packet s = case packet of
  Request kind fid
    | kind == sha1Info, Just f <- Map.lookup fid (files s), Just _ <- info f -> Just (fid, Nothing)
    | kind == sha1Chunk -> listToMaybe
        [ (file, Just n)
        | (file, f) <- Map.toList (files s), n <- Map.findWithDefault [] fid (index f), Set.member n (have f) ]
  _ -> Nothing

fileInfo :: FileId -> Swarm p -> Maybe Info
fileInfo fid = (>>= info) . Map.lookup fid . files

-- | What a peer says it has (HAVE, BITSET and HAVE_ALL) about files we
-- download, and its PC1 announcements, which we answer with what we have.
-- Peers that announce a file or ask for its chunks take part in it, so they
-- hear of the chunks we get.
heard :: Ord p => p -> Packet -> Swarm p -> (Swarm p, [Output p])
heard p packet s = case packet of
  Have kind fid chunks | kind == sha1Info -> (holding p (map fromIntegral chunks) fid s, [])
  Bitset kind fid first bits | kind == sha1Info -> (holding p [fromIntegral first + i | i <- [0 .. 8 * BS.length bits - 1], bit bits i] fid s, [])
  HaveAll kind fid | kind == sha1Info -> (s { files = Map.adjust (\f -> f { holders = Map.insert p Everything (holders f) }) fid (files s) }, [])
  Request kind h | kind == sha1Chunk -> (takingPart p h s, [])
  Announce ident
    | fid <- BS.drop 4 ident, announcing fid == ident, Just f <- Map.lookup fid (files s) ->
        (holding p [] fid s, [Send p h | Just inf <- [info f], not (Set.null (have f)), h <- haves fid inf (have f)])
  _ -> (s, [])
  where
    -- The first chunk is the top bit.
    bit bits i = testBit (BS.index bits (i `div` 8)) (7 - i `mod` 8)

-- | The peer has these chunks of the file, maybe none. Indices past the end
-- are left out, and without the info, we can't tell which there are.
holding :: Ord p => p -> [Int] -> FileId -> Swarm p -> Swarm p
holding p ns fid s = s { files = Map.adjust (\f -> f { holders = Map.alter (Just . more (maybe 0 Info.chunkCount (info f))) p (holders f) }) fid (files s) }
  where
    more n = \case
      Just Everything -> Everything
      old | theirs <- foldr Set.insert (known old) (filter (< n) ns) ->
        if n > 0 && Set.size theirs == n then Everything else Some theirs
    known = \case
      Just (Some theirs) -> theirs
      _                  -> Set.empty

-- | A peer that asks for or offers a chunk takes part in its file.
takingPart :: Ord p => p -> BS.ByteString -> Swarm p -> Swarm p
takingPart p h s = foldr (holding p []) s [fid | (fid, f) <- Map.toList (files s), Map.member h (index f)]

-- | Whether to take a transfer "Tox.Share.Transfer" offered: the info of a
-- file we download and haven't got the info of, or a chunk of one that we
-- miss and that isn't coming already.
offered :: Ord p => p -> TransferId -> FileKind -> BS.ByteString -> Word64 -> Swarm p -> (Bool, Swarm p)
offered p i kind fid size s0
  | kind == sha1Info, Just f <- Map.lookup fid (files s), isNothing (info f), size <= maxInfo, not (any isInfo (Map.elems (incoming s))) =
      (True, s { incoming = Map.insert (p, i) (IncomingInfo fid []) (incoming s) })
  | kind == sha1Chunk, (file, f, n : ns) : _ <- missing, fromIntegral size == maybe 0 (`Info.chunkLength` n) (info f) =
      (True, s { incoming = Map.insert (p, i) (IncomingChunk file (n : ns) []) (incoming s)
               , files = Map.insert file f { asked = foldr Map.delete (asked f) (n : ns) } (files s) })
  | otherwise = (False, s)
  where
    s | kind == sha1Chunk = takingPart p fid s0
      | otherwise = s0
    isInfo (IncomingInfo other _) = other == fid
    isInfo _                      = False
    coming = Set.fromList [(file, n) | IncomingChunk file ns _ <- Map.elems (incoming s), n <- ns]
    missing =
      [ (file, f, ns)
      | (file, f) <- Map.toList (files s), wanted f
      , let ns = [n | n <- Map.findWithDefault [] fid (index f), Set.notMember n (have f), Set.notMember (file, n) coming]
      , not (null ns) ]

-- | Bytes of a transfer we took, in order.
got :: Ord p => p -> TransferId -> BS.ByteString -> Swarm p -> Swarm p
got p i bytes s = s { incoming = Map.adjust add (p, i) (incoming s) }
  where
    add (IncomingInfo fid parts)     = IncomingInfo fid (bytes : parts)
    add (IncomingChunk fid ns parts) = IncomingChunk fid ns (bytes : parts)

-- | A transfer we took ended. What came is kept only if its SHA1 matches.
received :: Ord p => p -> TransferId -> Bool -> Swarm p -> (Swarm p, [Output p])
received p i ok s0 = case Map.lookup (p, i) (incoming s0) of
  Just (IncomingInfo fid parts)
    | ok, bytes <- BS.concat (reverse parts), Info.sha1 bytes == fid, Just inf <- Info.decode bytes ->
        (s { files = Map.adjust (\f -> indexed f { info = Just inf }) fid (files s) }, [Learned fid inf])
  Just (IncomingChunk fid ns@(n0 : _) parts)
    | ok, Just f <- Map.lookup fid (files s), Just inf <- info f, bytes <- BS.concat (reverse parts)
    , Just (Info.sha1 bytes) == Info.chunkHash inf n0 ->
        let done = Set.size (foldr Set.insert (have f) ns) == Info.chunkCount inf
            f' = f { have = foldr Set.insert (have f) ns, announce = if done then Nothing else announce f }
            tell = [Send q (Have sha1Info fid (map fromIntegral ns)) | q <- Map.keys (holders f), q /= p]
        in (s { files = Map.insert fid f' (files s) }, [Chunk fid n bytes | n <- ns] <> tell <> [Finished fid | done])
  _ -> (s, [])
  where s = s0 { incoming = Map.delete (p, i) (incoming s0) }

-- | The file of a chunk a transfer we took brings.
receiving :: Ord p => p -> TransferId -> Swarm p -> Maybe FileId
receiving p i s = case Map.lookup (p, i) (incoming s) of
  Just (IncomingChunk fid _ _) -> Just fid
  _                            -> Nothing

-- | Asks for what's missing: each file's info from a peer that has it, and
-- chunks from the peers that have them, up to 'maxRequests' each and a
-- third of the file, as PickerStrategyRandomSequential picks them. Files take
-- turns going first, and wait more ticks between picks the lower their
-- 'Priority'.
tick :: Ord p => Seconds -> Swarm p -> (Swarm p, [Output p])
tick now s0 = foldr announceFile (s2 { turn = listToMaybe (drop 1 order) }, asks) order
  where
    s1 = s0 { files = Map.map (\f -> f { asked = Map.filter ((> now - chunkRetry) . snd) (asked f) }) (files s0) }
    order = let (before, after) = span ((< turn s0) . Just) (Map.keys (files s1)) in after <> before
    (s2, asks, _, _) = foldl askFile (s1, [], open, infos) order
    open = Map.fromListWith (+) ([(p, 1 :: Int) | f <- Map.elems (files s1), (p, _) <- Map.elems (asked f)] <> [(p, 1) | ((p, _), IncomingChunk {}) <- Map.toList (incoming s1)])
    infos = length [() | IncomingInfo {} <- Map.elems (incoming s1)] + length [() | f <- Map.elems (files s1), isNothing (info f), Just t <- [infoAsked f], t > now - infoRetry]
    announceFile fid (s, outs) = case Map.lookup fid (files s) of
      Just f | Just (at, gap) <- announce f, at <= now ->
        (s { files = Map.insert fid f { announce = Just (now + gap, 2 * gap) } (files s) }, SendAll (Announce (announcing fid)) : outs)
      _ -> (s, outs)
    askFile acc@(s, outs, counts, asking) fid = case Map.lookup fid (files s) of
      Nothing -> acc
      Just f -> case info f of
        Nothing
          | asking < maxInfoRequests, maybe True (<= now - infoRetry) (infoAsked f), not (Map.null (holders f)) ->
              let (k, g) = uniformR (0, Map.size (holders f) - 1) (gen s)
                  p = fst (Map.elemAt k (holders f))
              in (s { files = Map.insert fid f { infoAsked = Just now } (files s), gen = g }, outs <> [Send p (Request sha1Info fid)], counts, asking + 1)
          | otherwise -> acc
        Just inf
          | not (wanted f) -> acc
          | waited f < waits (priority f) -> (s { files = Map.insert fid f { waited = waited f + 1 } (files s) }, outs, counts, asking)
          | otherwise ->
              let coming = Set.fromList [n | IncomingChunk other ns _ <- Map.elems (incoming s), other == fid, n <- ns]
                  needed n = Set.notMember n (have f) && Map.notMember n (asked f) && Set.notMember n coming
                  (picked, counts', g) = foldl (pick f inf needed) ([], counts, gen s) (Map.toList (holders f))
                  f' = f { asked = foldr (\(p, n) -> Map.insert n (p, now)) (asked f) picked, waited = 0 }
              in (s { files = Map.insert fid f' (files s), gen = g }, outs <> [Send p (Request sha1Chunk h) | (p, n) <- picked, Just h <- [Info.chunkHash inf n]], counts', asking)
    pick f inf needed (picked, counts, g) (p, held) =
      let count = Info.chunkCount inf
          room = min (maxRequests - Map.findWithDefault 0 p counts) (max 1 (count `div` 3))
          (from, g1) = uniformR (0, count - 1) g
          headAt = fromIntegral (readHead f `div` fromIntegral (Info.chunkSize inf))
          taken = Set.fromList (map snd picked)
          has n = case held of
            Everything  -> True
            Some theirs -> Set.member n theirs
          wanting n = has n && needed n && Set.notMember n taken
          (ns, g') = mixed room g1 (filter wanting ([from .. count - 1] <> [0 .. from - 1])) (filter wanting [if headAt < count then headAt else 0 .. count - 1])
      in (picked <> map (p,) ns, Map.insertWith (+) p (length ns) counts, g')

-- | Up to this many, each the next of one of the two lists by a coin toss
-- (or of the other, once one runs out).
mixed :: Int -> StdGen -> [Int] -> [Int] -> ([Int], StdGen)
mixed n g xs ys
  | n <= 0 = ([], g)
  | otherwise = case (if heads then xs else ys) <> xs <> ys of
      []    -> ([], g')
      x : _ | (rest, g'') <- mixed (n - 1) g' (filter (/= x) xs) (filter (/= x) ys) -> (x : rest, g'')
  where (heads, g') = uniform g

-- | What a PC1 announcement of a file says: its kind, then its ID.
announcing :: FileId -> BS.ByteString
announcing = (BS.pack [fromIntegral (k `shiftR` n) | n <- [0, 8, 16, 24]] <>)
  where FileKind k = sha1Info

-- | HAVE_ALL, or the chunks we have as BITSETs.
haves :: FileId -> Info -> Set.Set Int -> [Packet]
haves fid inf ours
  | Set.size ours == count = [HaveAll sha1Info fid]
  | otherwise = [Bitset sha1Info fid (fromIntegral first) (bits first) | first <- [0, bitsetChunks .. count - 1]]
  where
    count = Info.chunkCount inf
    bits first = BS.pack
      [ foldr (\j b -> if Set.member (first + 8 * byte + j) ours then setBit b (7 - j) else b) 0 [0 .. 7]
      | byte <- [0 .. (min bitsetChunks (count - first) + 7) `div` 8 - 1] ]


-- | A peer joined the group, which may have what we download: we announce
-- our downloads sooner, as sha1_ngcft1.cpp does.
joined :: Seconds -> Swarm p -> Swarm p
joined now s = s { files = Map.map (\f -> f { announce = sooner <$> announce f }) (files s) }
  where sooner (at, gap) = (now + 0.1 * max 0 (at - now), 0.9 * gap)

-- | The peer left the group.
leave :: Ord p => p -> Swarm p -> Swarm p
leave p s = s
  { files = Map.map (\f -> f { holders = Map.delete p (holders f), asked = Map.filter ((/= p) . fst) (asked f) }) (files s)
  , incoming = Map.filterWithKey (\(q, _) _ -> q /= p) (incoming s)
  }
