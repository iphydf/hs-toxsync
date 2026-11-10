{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE LambdaCase #-}
module Tox.Share.PacketSpec (spec) where

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8  as Char8
import           System.Directory       (doesFileExist)
import           Test.Hspec
import           Test.QuickCheck

import           Tox.Share.Packet

-- | Reads the file from Bazel's runfiles, or from this package.
recorded :: IO [(Channel, BS.ByteString)]
recorded = go ["hs-toxsync/test/share/recorded.txt", "test/share/recorded.txt"]
  where
    go (path : rest) = doesFileExist path >>= \case
      True  -> concatMap line . Char8.lines <$> BS.readFile path
      False -> go rest
    go [] = fail "No recorded.txt"
    line l = case Char8.unpack <$> Char8.words l of
      ["public", h]  -> [(Public, hex h)]
      ["private", h] -> [(Private, hex h)]
      _              -> []

instance Arbitrary FileKind where
  arbitrary = FileKind <$> arbitrary

instance Arbitrary Packet where
  arbitrary = oneof
    [ Request <$> arbitrary <*> bytes
    , Init <$> arbitrary <*> arbitrary <*> arbitrary <*> bytes
    , Init2 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> bytes
    , InitAck <$> arbitrary <*> arbitrary <*> arbitrary
    , Data <$> arbitrary <*> arbitrary <*> bytes
    , DataAck <$> arbitrary <*> arbitrary
    , Message <$> arbitrary <*> arbitrary <*> bytes
    , Have <$> arbitrary <*> bytes <*> arbitrary
    , Bitset <$> arbitrary <*> bytes <*> arbitrary <*> bytes
    , HaveAll <$> arbitrary <*> (BS.cons <$> arbitrary <*> bytes)
    , Announce <$> bytes
    ]
    where bytes = BS.pack <$> arbitrary

-- | The channel a packet may always come on.
channelFor :: Packet -> Channel
channelFor = \case
  Message {} -> Public
  _          -> Private

hex :: String -> BS.ByteString
hex = either error id . Base16.decode . Char8.pack

spec :: Spec
spec = do
  it "reads every packet Green-Sky's clients sent, and writes them back the same" $ do
    packets <- recorded
    packets `shouldNotSatisfy` null
    mapM_ (\(channel, bytes) -> (encode <$> decode channel bytes) `shouldBe` Just bytes) packets

  it "reads what it writes" $ property $ \p ->
    decode (channelFor p) (encode p) `shouldBe` Just p

  it "reads a two-byte packet that doesn't also read as HAVE_ALL" $
    decode Private (hex "900507") `shouldBe` Just (DataAck 7 [])

  it "takes a two-byte packet that also reads as HAVE_ALL for one, as ngcext does" $
    -- A DATA packet in the two-byte form: 0x90, 0x04, transfer 7, sequence 1, data.
    decode Private (hex "900407010061") `shouldBe` Just (HaveAll (FileKind 0x00010704) (hex "61"))

  it "rejects packets on a channel they can't come on" $ do
    decode Private (encode (Message 1 sha1Info (hex "00"))) `shouldBe` Nothing
    decode Public (encode (Data 1 2 (hex "00"))) `shouldBe` Nothing
    decode Public (encode (Request sha1Info (hex "00"))) `shouldBe` Just (Request sha1Info (hex "00"))

  it "fills in what older INIT_ACKs leave out" $ do
    decode Private (hex "8a05") `shouldBe` Just (InitAck 5 496 0)
    decode Private (hex "8a050102") `shouldBe` Just (InitAck 5 0x0201 0)
    decode Private (hex "8a05010203") `shouldBe` Just (InitAck 5 0x0201 3)

  it "rejects packets cut short or with a file ID longer than the packet" $ do
    decode Private (hex "8c0501") `shouldBe` Nothing
    decode Private (hex "8e090000000100aa010203") `shouldBe` Nothing
    decode Private (hex "8e09000000ffffaa") `shouldBe` Nothing
    decode Private (hex "9009000000") `shouldBe` Nothing
    decode Private (hex "8901000000") `shouldBe` Nothing
