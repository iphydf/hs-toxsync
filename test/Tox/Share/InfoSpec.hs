module Tox.Share.InfoSpec (spec) where

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8  as Char8
import           System.Directory       (doesFileExist)
import           Test.Hspec

import           Tox.Share.Info

-- | Reads the file from Bazel's runfiles, or from this package.
recorded :: IO BS.ByteString
recorded = go ["hs-toxsync/test/share/info.hex", "test/share/info.hex"]
  where
    go (path : rest) = doesFileExist path >>= \found ->
      if found then either error id . Base16.decode . last . Char8.lines <$> BS.readFile path else go rest
    go [] = fail "No info.hex"

hex :: String -> BS.ByteString
hex = either error id . Base16.decode . Char8.pack

spec :: Spec
spec = do
  it "reads the info totato sent, whose SHA1 is the file ID it announced" $ do
    bytes <- recorded
    i <- maybe (fail "no info") pure (decode bytes)
    (name i, size i, chunkSize i, chunkCount i) `shouldBe` (Char8.pack "payload.bin", 300000, 32768, 10)
    sha1 bytes `shouldBe` hex "be44291f8b1a4b5fe99c7941e0a7b3cf8c9e6bdc"
    encode i `shouldBe` bytes

  it "picks the chunk size totato picked, and the last chunk is the rest" $ do
    bytes <- recorded
    i <- maybe (fail "no info") pure (decode bytes)
    chunkSizeFor (size i) `shouldBe` chunkSize i
    chunkLength i 9 `shouldBe` 300000 - 9 * 32768

  it "picks larger chunks for larger files, as ngcft1 does" $ do
    chunkSizeFor (512 * 1024) `shouldBe` 32 * 1024
    chunkSizeFor (100 * 1024 * 1024) `shouldBe` 256 * 1024
    chunkSizeFor (3 * 1024 * 1024 * 1024) `shouldBe` 4 * 1024 * 1024

  it "rejects an info whose chunks don't cover its size" $ do
    bytes <- recorded
    decode (BS.dropEnd 20 bytes) `shouldBe` Nothing
    decode (BS.dropEnd 1 bytes) `shouldBe` Nothing
