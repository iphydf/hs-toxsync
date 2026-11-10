module Tox.Share.HistorySpec (spec) where

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8  as Char8
import qualified Data.Text              as Text
import           System.Directory       (doesFileExist)
import           System.Random          (mkStdGen)
import           Test.Hspec

import qualified Tox.Share              as Share
import           Tox.Share              (Share)
import           Tox.Share.History
import qualified Tox.Share.Packet       as Packet
import           Tox.Share.Packet       (Channel (..), sha1Info)

-- | Reads the file from Bazel's runfiles, or from this package.
recorded :: IO BS.ByteString
recorded = go ["hs-toxsync/test/share/chatlog.hex", "test/share/chatlog.hex"]
  where
    go (path : rest) = doesFileExist path >>= \found ->
      if found then either error id . Base16.decode . last . Char8.lines <$> BS.readFile path else go rest
    go [] = fail "No chatlog.hex"

hex :: String -> BS.ByteString
hex = either error id . Base16.decode . Char8.pack

data Side = A | B
  deriving (Eq, Ord, Show)

-- | Hands each side's packets to the other, ticking both, for 10 s. The
-- first side starts with these outputs. What each side output comes back.
exchange :: (Side, Share Side) -> (Side, Share Side) -> [Share.Output Side] -> ([Share.Output Side], [Share.Output Side])
exchange (x, sx0) (y, sy0) started = go (0 :: Int) sx0 sy0 started [] [] []
  where
    go n sx sy toY seenX seenY toX
      | n > 2000 = (seenX, seenY)
      | otherwise =
          let t = fromIntegral n * 0.005
              (sy1, outY) = foldl (deliver t x) (sy, []) toY
              (sx1, outX) = foldl (deliver t y) (sx, []) toX
              (sx2, tickX) = Share.tick t sx1
              (sy2, tickY) = Share.tick t sy1
          in go (n + 1) sx2 sy2 (outX <> tickX) (seenX <> outX <> tickX) (seenY <> outY <> tickY) (outY <> tickY)
    deliver t from (s, acc) o = case o of
      Share.Send _ p -> let (s', o') = Share.receive t from Private (Packet.encode p) s in (s', acc <> o')
      _              -> (s, acc)

spec :: Spec
spec = do
  it "reads the chatlog totato sent, and writes it back the same" $ do
    bytes <- recorded
    entries <- maybe (fail "unreadable") pure (decodeLog bytes)
    length entries `shouldBe` 7
    take 1 (map body entries) `shouldBe` [Shared sha1Info (hex "be44291f8b1a4b5fe99c7941e0a7b3cf8c9e6bdc")]
    [t | Entry { body = Text False t } <- entries] `shouldContain` map Text.pack ["third message", "second message", "hello from a"]
    encodeLog entries `shouldBe` bytes

  it "reads the range of totato's request, and refuses one that ends before it starts" $ do
    decodeRange (hex "351af36a000000003577f06a00000000") `shouldBe` Just (Range 1794316853 1794144053)
    decodeRange (hex "3577f06a00000000351af36a00000000") `shouldBe` Nothing

  it "asks a peer for the last two days, and gets the messages it sends back" $ do
    let sent = [Entry 17943168530 (BS.replicate 32 1) 7 (Text False (Text.pack "hi")), Entry 17943168520 (BS.replicate 32 2) 8 (Text True (Text.pack "waves"))]
        (a, asks) = Share.requestHistory 1794316853 B (Share.new 0 1369 (mkStdGen 1))
        b = Share.new 0 1369 (mkStdGen 2)
        (_, seenB) = exchange (A, a) (B, b) asks
        wanted = [r | Share.HistoryWanted A r <- seenB]
    wanted `shouldBe` [lastTwoDays 1794316853]
    let (b', answer) = Share.answerHistory 0 A (lastTwoDays 1794316853) sent b
        (_, seenA) = exchange (B, b') (A, a) answer
    [es | Share.History B es <- seenA] `shouldBe` [sent]
