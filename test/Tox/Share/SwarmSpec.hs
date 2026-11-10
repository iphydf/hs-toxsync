module Tox.Share.SwarmSpec (spec) where

import qualified Data.ByteString      as BS
import           Data.List            (mapAccumL, partition, sortOn)
import qualified Data.Map.Strict      as Map
import           System.Random        (StdGen, mkStdGen, uniformR)
import           Test.Hspec

import qualified Tox.Share            as Share
import           Tox.Share            (Share)
import           Tox.Share.Congestion (Seconds)
import qualified Tox.Share.Info       as Info
import           Tox.Share.Info       (Info (Info))
import qualified Tox.Share.Packet     as Packet
import           Tox.Share.Packet     (Channel (..), sha1Chunk, sha1Info)
import qualified Tox.Share.Stats      as Stats
import qualified Tox.Share.Swarm      as Swarm

data Side = A | B | C
  deriving (Eq, Ord, Show, Enum)

payload :: BS.ByteString
payload = BS.pack (take 300000 (cycle [0 .. 250]))

-- | In more chunks than a peer asks for at once, so that peers downloading
-- together have chunks the other hasn't asked for yet.
chunkSize :: Int
chunkSize = 8192

info :: Info
info = Info (BS.pack [104, 105]) (fromIntegral (BS.length payload)) (fromIntegral chunkSize) (BS.concat [Info.sha1 (chunk n) | n <- [0 .. count - 1]])
  where count = (BS.length payload + chunkSize - 1) `div` chunkSize

chunk :: Int -> BS.ByteString
chunk n = BS.take chunkSize (BS.drop (n * chunkSize) payload)

-- | One side of the group, the chunks it got, and from whom.
data Node = Node
  { share    :: Share Side
  , stored   :: Map.Map Int BS.ByteString
  , from     :: [Side]
  , finished :: Bool
  }

data Run = Run
  { nodes :: Side -> Node
  , wire  :: [(Seconds, Side, Side, Channel, BS.ByteString)]
    -- ^ When, from, to.
  , gen   :: StdGen
  }

-- | A shares 'payload', the downloaders download it once A's message comes,
-- losing this share of lossy packets on links with 50 ms delay.
simulate :: [Side] -> Double -> Seconds -> Run
simulate downloaders loss end = go 0 (foldl (output 0 A) (Run node [] (mkStdGen 3)) shared)
  where
    sides = A : downloaders
    (a0, shared) = Share.share 1 info (Share.new 0 1369 (mkStdGen 1))
    node A = Node a0 Map.empty [] False
    node s = Node (Share.new 0 1369 (mkStdGen (fromEnum s + 10))) Map.empty [] False
    go now r
      | now > end || all (finished . nodes r) downloaders = r
      | otherwise =
          let (due, later) = partition (\(t, _, _, _, _) -> t <= now) (wire r)
              arrive acc (_, q, s, channel, bytes) = step now s (Share.receive now q channel bytes) acc
              r1 = foldl arrive r { wire = later } (sortOn (\(t, _, _, _, _) -> t) due)
          in go (now + 0.005) (foldl (\acc s -> step now s (Share.tick now) acc) r1 sides)
    step now s f r = let (sh, outs) = f (share (nodes r s)) in foldl (output now s) (with s (nodes r s) { share = sh } r) outs
    with s n r = r { nodes = \x -> if x == s then n else nodes r x }
    output now s r o = let n = nodes r s in case o of
      Share.Send q p -> send now s q Private p r
      Share.SendAll p -> foldl (\acc q -> send now s q Public p acc) r (filter (/= s) sides)
      Share.Shared q _ fid -> with s n { share = Share.download q fid (share n) } r
      Share.Chunk _ c bytes -> with s n { stored = Map.insert c bytes (stored n) } r
      Share.ChunkWanted q fid c -> step now s (Share.sendChunk now q fid c (if s == A then chunk c else stored n Map.! c)) r
      Share.Finished _ -> with s n { finished = True } r
      _ -> r
    send now q to channel p r =
      let (lost, g) = uniformR (0, 1) (gen r)
          r' = case p of
            Packet.Data {} -> let n = nodes r to in with to n { from = q : from n } r
            _              -> r
      in if not (Packet.lossless p) && lost < loss then r { gen = g } else r' { gen = g, wire = wire r <> [(now + 0.05, q, to, channel, Packet.encode p)] }

spec :: Spec
spec = do
  it "downloads a shared file's info, then every chunk, checked" $ do
    let r = simulate [B] 0 60
        b = nodes r B
        counted s = Share.stats 0 (Info.hash info) (share (nodes r s))
    finished b `shouldBe` True
    BS.concat (Map.elems (stored b)) == payload `shouldBe` True
    (Stats.totalUp (counted A), Stats.totalDown (counted B)) `shouldSatisfy` \(up, down) -> up >= down && down == fromIntegral (BS.length payload)

  it "downloads it whole when lossy packets are lost" $ do
    let b = nodes (simulate [B] 0.1 120) B
    finished b `shouldBe` True
    BS.concat (Map.elems (stored b)) == payload `shouldBe` True

  it "has peers downloading together swap chunks" $ do
    let r = simulate [B, C] 0 60
    [finished (nodes r s) | s <- [B, C]] `shouldBe` [True, True]
    [BS.concat (Map.elems (stored (nodes r s))) == payload | s <- [B, C]] `shouldBe` [True, True]
    [q `elem` from (nodes r s) | (s, q) <- [(B, C), (C, B)]] `shouldBe` [True, True]

  it "tells a peer that announced the file, but has nothing, of each chunk it gets" $ do
    let fid = Info.hash info
        announced = Packet.Announce (BS.pack [9, 0, 0, 0] <> fid)
        (s1, _) = Swarm.heard B announced (learned (Swarm.download A fid none))
        (_, s2) = Swarm.offered A 1 sha1Chunk (Info.chunks info !! 0) (fromIntegral (BS.length (chunk 0))) s1
    snd (Swarm.received A 1 True (Swarm.got A 1 (chunk 0) s2)) `shouldContain` [Swarm.Send B (Packet.Have sha1Info fid [0])]

  it "announces sooner when a peer joins" $ do
    let fid = Info.hash info
        announced = Swarm.SendAll (Packet.Announce (BS.pack [9, 0, 0, 0] <> fid))
        started = fst (Swarm.tick 0 (Swarm.download A fid none))
    announced `elem` snd (Swarm.tick 11 started) `shouldBe` False
    announced `elem` snd (Swarm.tick 11 (Swarm.joined 5 started)) `shouldBe` True

  it "answers a PC1 announcement with what it has" $ do
    let fid = Info.hash info
        announced = Packet.Announce (BS.pack [9, 0, 0, 0] <> fid)
        partial = foldl (\s n -> got n s) (learned (Swarm.download A fid none)) [0, 2]
        got n s =
          let (_, s1) = Swarm.offered A (fromIntegral n + 1) sha1Chunk (Info.chunks info !! n) (fromIntegral (BS.length (chunk n))) s
          in fst (Swarm.received A (fromIntegral n + 1) True (Swarm.got A (fromIntegral n + 1) (chunk n) s1))
    snd (Swarm.heard B announced (Swarm.share fid info none)) `shouldBe` [Swarm.Send B (Packet.HaveAll sha1Info fid)]
    snd (Swarm.heard B announced partial) `shouldBe` [Swarm.Send B (Packet.Bitset sha1Info fid 0 (BS.pack [0xa0, 0, 0, 0, 0]))]

  it "announces a file it downloads, at once and then less and less often" $ do
    let fid = Info.hash info
        announced = Swarm.SendAll (Packet.Announce (BS.pack [9, 0, 0, 0] <> fid))
        times = [t | t <- [0, 10 .. 400], announced `elem` snd (Swarm.tick t (ticked t))]
        ticked t = foldl (\s u -> fst (Swarm.tick u s)) (Swarm.download A fid none) (takeWhile (< t) [0, 10 .. 400])
    times `shouldBe` [0, 60, 180]

  it "downloads by the info alone, from the peers that answer its announcement" $ do
    let fid = Info.hash info
        (asked, announced) = Swarm.tick 0 (Swarm.fetch info none)
        answered = fst (Swarm.heard B (Packet.HaveAll sha1Info fid) asked)
    announced `shouldBe` [Swarm.SendAll (Packet.Announce (BS.pack [9, 0, 0, 0] <> fid))]
    [h `elem` Info.chunks info | Swarm.Send B (Packet.Request kind h) <- ticks [1, 2, 3] answered, kind == sha1Chunk] `shouldSatisfy` \asks -> not (null asks) && and asks

  it "asks for nothing of a paused file until it's fetched again" $ do
    let fid = Info.hash info
        paused = Swarm.pause fid (fst (Swarm.heard B (Packet.HaveAll sha1Info fid) (Swarm.fetch info none)))
    [h | Swarm.Send _ (Packet.Request _ h) <- ticks [1 .. 10] paused] `shouldBe` []
    ticks [1 .. 10] (Swarm.fetch info paused) `shouldNotBe` []

  it "gives a peer's request slots to the file of higher priority" $ do
    let other = Info (BS.pack [104, 111]) (Info.size info) (Info.chunkSize info) (BS.map (+ 1) (Info.hashes info))
        both = foldr (\inf -> fst . Swarm.heard B (Packet.HaveAll sha1Info (Info.hash inf)) . Swarm.fetch inf) none [info, other]
        prioritised = Swarm.prioritise Swarm.Lowest (Info.hash info) (Swarm.prioritise Swarm.Highest (Info.hash other) both)
        asked = [h | Swarm.Send B (Packet.Request kind h) <- ticks [1 .. 10] prioritised, kind == sha1Chunk]
    length asked `shouldBe` 5
    all (`elem` Info.chunks other) asked `shouldBe` True

  it "asks for chunks from where the client reads" $ do
    let fid = Info.hash info
        reading = Swarm.readFrom (20 * fromIntegral (Info.chunkSize info)) fid (fst (Swarm.heard B (Packet.HaveAll sha1Info fid) (Swarm.fetch info none)))
    [h | Swarm.Send B (Packet.Request _ h) <- ticks [1 .. 10] reading] `shouldContain` [Info.chunks info !! 20]

  it "asks for the info of a few files at once" $ do
    let fids = [BS.replicate 20 n | n <- [1 .. 10]]
        learnt = foldr (Swarm.learn B) none fids
    length [h | Swarm.Send B (Packet.Request kind h) <- ticks [1] learnt, kind == sha1Info] `shouldBe` 6

  it "drops a chunk whose bytes don't match its SHA1" $ do
    let fid = Info.hash info
        (taken, s) = Swarm.offered A 7 sha1Chunk (Info.chunks info !! 3) (fromIntegral (BS.length (chunk 3))) (learned (Swarm.download A fid none))
        (_, outs) = Swarm.received A 7 True (Swarm.got A 7 (BS.map (+ 1) (chunk 3)) s)
    taken `shouldBe` True
    outs `shouldBe` []
  where
    none = Swarm.empty (mkStdGen 0)
    -- What the ticks at these times send.
    ticks ts s0 = concat (snd (mapAccumL (flip Swarm.tick) s0 ts))
    -- As if A sent B the info.
    learned s0 =
      let size = fromIntegral (BS.length (Info.encode info))
          (_, s1) = Swarm.offered A 0 sha1Info (Info.hash info) size s0
      in fst (Swarm.received A 0 True (Swarm.got A 0 (Info.encode info) s1))
