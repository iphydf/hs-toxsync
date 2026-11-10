module Tox.Share.TransferSpec (spec) where

import qualified Data.ByteString      as BS
import           Data.List            (partition, sortOn)
import           Data.Maybe           (fromMaybe)
import           System.Random        (StdGen, mkStdGen, uniformR)
import           Test.Hspec

import           Tox.Share.Congestion (Seconds)
import           Tox.Share.Packet     (Channel (..), FileKind (..), Packet (..),
                                       decode, encode, lossless)
import           Tox.Share.Transfer

data Side = A | B
  deriving (Eq, Ord, Show)

other :: Side -> Side
other A = B
other B = A

-- | How lossy packets fare: lost, sent twice, and up to how much later
-- than the delay. Lossless ones always come, in order.
data Link = Link
  { delay  :: Seconds
  , loss   :: Double
  , twice  :: Double
  , jitter :: Seconds
  }

data Run = Run
  { transfers :: Side -> Transfers Side
  , wire      :: [(Seconds, Side, Packet)]
  , outputs   :: [(Seconds, Side, Output Side)]
  , gen       :: StdGen
  }

payload :: BS.ByteString
payload = BS.pack (take 300000 (cycle [0 .. 250]))

-- | A sends 'payload' to B, which takes every file offered, ticking both
-- every step until the time given or until it's over.
simulate :: Link -> Seconds -> Seconds -> Run
simulate link step end = go 0 (foldl (emit 0 A) run0 (snd started))
  where
    fresh s = new 0 1369 (mkStdGen (if s == A then 1 else 2))
    run0 = Run (\s -> if s == A then fst started else fresh B) [] [] (mkStdGen 3)
    go now r
      | now > end || sent r = r
      | otherwise =
          let (due, later) = partition (\(t, _, _) -> t <= now) (wire r)
              r1 = foldl (\acc (_, to, p) -> handle now to (receive now (other to) p (transfers acc to)) acc) r { wire = later } (sortOn (\(t, _, _) -> t) due)
          in go (now + step) (foldl (\acc s -> handle now s (tick now (transfers acc s)) acc) r1 [A, B])
    sent r = not (null [() | (_, _, Sent {}) <- outputs r])
    handle now s (t, outs) r = foldl (emit now s) r { transfers = \x -> if x == s then t else transfers r x } outs
    emit now s r o = case o of
      Send to p ->
        let bytes = encode p
            p' = fromMaybe (error "undecodable") (decode Private bytes)
            (lost, g1) = uniformR (0, 1) (gen r)
            (dup, g2) = uniformR (0, 1) g1
            (late, g3) = uniformR (0, jitter link) g2
            copies
              | lossless p' = [now + delay link]
              | lost < loss link = []
              | dup < twice link = [now + delay link + late, now + delay link + late / 2]
              | otherwise = [now + delay link + late]
        in r { wire = wire r <> [(t, to, p') | t <- copies], gen = g3, outputs = outputs r <> [(now, s, o)] }
      Offered from i _ _ _ ->
        handle now s (accept now from i (transfers r s)) r { outputs = outputs r <> [(now, s, o)] }
      _ -> r { outputs = outputs r <> [(now, s, o)] }

received :: Run -> BS.ByteString
received r = BS.concat [bytes | (_, B, Got _ _ _ bytes) <- outputs r]

ended :: Run -> [(Side, Output Side)]
ended r = [(s, o) | (_, s, o) <- outputs r, isEnd o]
  where
    isEnd (Sent {})     = True
    isEnd (Received {}) = True
    isEnd _             = False

-- | A's transfers after starting to send B 'payload'.
started :: (Transfers Side, [Output Side])
started = maybe (error "no transfer") snd $
  start 0 B (FileKind 9) (BS.replicate 20 7) (fromIntegral (BS.length payload)) (\off n -> BS.take n (BS.drop (fromIntegral off) payload)) (new 0 1369 (mkStdGen 1))

-- | B's transfers after taking A's offer of transfer 0.
taken :: Transfers Side
taken = fst (accept 0 A 0 (fst (receive 0 A (Init (FileKind 9) 300000 0 (BS.replicate 20 7)) (new 0 1369 (mkStdGen 2)))))

spec :: Spec
spec = do
  it "sends a file whole over a link that keeps every packet" $ do
    let r = simulate (Link 0.05 0 0 0) 0.005 30
    (BS.length (received r), received r == payload) `shouldBe` (BS.length payload, True)
    ended r `shouldBe` [(B, Received A 0 True), (A, Sent B 0 True)]

  it "sends a file whole when packets are lost, repeated and reordered" $ do
    let r = simulate (Link 0.05 0.1 0.05 0.03) 0.005 120
    (BS.length (received r), received r == payload) `shouldBe` (BS.length payload, True)
    ended r `shouldBe` [(B, Received A 0 True), (A, Sent B 0 True)]

  it "offers a file 8 times to a peer that never answers, then gives up" $ do
    let (_, outs) = foldl (\(t, os) now -> (<> os) <$> tick now t) started [1 .. 300]
    length [() | Send _ Init {} <- outs] `shouldBe` 8
    [o | o@Sent {} <- outs] `shouldBe` [Sent B 0 False]

  it "holds at most 1024 packets past a gap, whatever the peer sends" $ do
    let flooded = foldl (\t sequ -> fst (receive 1 A (Data 0 sequ (BS.replicate 1000 1)) t)) taken [1 .. 5000]
    held flooded `shouldBe` 1023 * 1000

  it "runs nothing once it declines an offer" $ do
    let offered = fst (receive 0 A (Init (FileKind 9) 300000 0 (BS.replicate 20 7)) (new 0 1369 (mkStdGen 2)))
    (active offered, active (decline A 0 offered)) `shouldBe` (True, False)

  it "ends every transfer with a peer that leaves" $ do
    snd (leave B (fst started)) `shouldBe` [Sent B 0 False]
    snd (leave A taken) `shouldBe` [Received A 0 False]
