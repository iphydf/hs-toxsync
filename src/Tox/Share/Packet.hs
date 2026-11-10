{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
-- | Packets of Green-Sky's NGC extensions (ngcext in solanaceae_ngc_ft1), sent
-- as group custom packets: file transfers (FT1) and participation
-- announcements (PC1). Numbers are little-endian.
module Tox.Share.Packet
  ( Packet (..)
  , FileKind (..)
  , TransferId
  , Channel (..)
  , sha1Info
  , sha1Chunk
  , historyRange
  , decode
  , encode
  , lossless
  ) where

import           Control.Applicative     ((<|>))
import           Control.Monad           (guard)
import           Data.Binary.Get         (Get, getByteString,
                                          getRemainingLazyByteString,
                                          getWord16le, getWord32le, getWord64le,
                                          getWord8, isEmpty, runGetOrFail)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as BL
import           Data.List               (find, uncons)
import           Data.Word               (Word16, Word32, Word64, Word8)

newtype FileKind = FileKind Word32
  deriving (Eq, Ord, Show)

-- | The kinds current clients send (the old numbering in ngcft1_file_kind.hpp).
sha1Info, sha1Chunk, historyRange :: FileKind
sha1Info = FileKind 9
sha1Chunk = FileKind 13
historyRange = FileKind 0x0f02

-- | Picked by the sender, per peer.
type TransferId = Word8

data Channel = Public | Private
  deriving (Eq, Show)

data Packet
  = Request FileKind BS.ByteString
  | Init FileKind Word64 TransferId BS.ByteString
    -- ^ Starts sending a file of this size (0 for unknown).
  | Init2 FileKind Word64 TransferId Word8 BS.ByteString
    -- ^ 'Init' with feature flags.
  | InitAck TransferId Word16 Word8
    -- ^ Accepts a transfer, with the most data each 'Data' may carry and
    -- feature flags.
  | Data TransferId Word16 BS.ByteString
  | DataAck TransferId [Word16]
  | Message Word32 FileKind BS.ByteString
    -- ^ Shares a file in the group, with the message's ID.
  | Have FileKind BS.ByteString [Word32]
    -- ^ Indices of the chunks the sender has.
  | Bitset FileKind BS.ByteString Word32 BS.ByteString
    -- ^ Which chunks the sender has, from the first one's index on, a bit each.
  | HaveAll FileKind BS.ByteString
  | Announce BS.ByteString
    -- ^ The sender takes part in this (PC1).
  deriving (Eq, Show)

data Kind
  = RequestKind
  | InitKind
  | Init2Kind
  | InitAckKind
  | DataKind
  | DataAckKind
  | MessageKind
  | HaveKind
  | BitsetKind
  | HaveAllKind
  | AnnounceKind
  deriving (Eq, Enum, Bounded)

kindOf :: Packet -> Kind
kindOf = \case
  Request {}  -> RequestKind
  Init {}     -> InitKind
  Init2 {}    -> Init2Kind
  InitAck {}  -> InitAckKind
  Data {}     -> DataKind
  DataAck {}  -> DataAckKind
  Message {}  -> MessageKind
  Have {}     -> HaveKind
  Bitset {}   -> BitsetKind
  HaveAll {}  -> HaveAllKind
  Announce {} -> AnnounceKind

-- | The one-byte ID current clients send.
oldId :: Kind -> Word8
oldId = \case
  RequestKind  -> 0x88
  InitKind     -> 0x89
  InitAckKind  -> 0x8a
  DataKind     -> 0x8b
  DataAckKind  -> 0x8c
  MessageKind  -> 0x8d
  HaveKind     -> 0x8e
  BitsetKind   -> 0x8f
  HaveAllKind  -> 0x90
  Init2Kind    -> 0x91
  AnnounceKind -> 0xa0

-- | The ID after 'newPrefix' in the newer two-byte form.
newId :: Kind -> Word8
newId = \case
  RequestKind  -> 0x00
  InitKind     -> 0x01
  Init2Kind    -> 0x02
  InitAckKind  -> 0x03
  DataKind     -> 0x04
  DataAckKind  -> 0x05
  MessageKind  -> 0x06
  HaveKind     -> 0x07
  BitsetKind   -> 0x08
  HaveAllKind  -> 0x09
  AnnounceKind -> 0x10

newPrefix :: Word8
newPrefix = 0x90

-- | What 'InitAck' means when it leaves out the size, from before it had one.
defaultMaxData :: Word16
defaultMaxData = 496

-- | 'Nothing' for packets that aren't ours, are cut short, or came on a
-- channel they can't come on. The one-byte IDs are tried first, as ngcext
-- does, so a two-byte packet starting with 'newPrefix' that also reads as
-- 'HaveAll' is one.
decode :: Channel -> BS.ByteString -> Maybe Packet
decode channel bytes = withId oldId 1 <|> (guard (BS.take 1 bytes == BS.singleton newPrefix) >> withId newId 2)
  where
    withId ids n = do
      i <- BS.indexMaybe bytes (n - 1)
      kind <- find ((== i) . ids) [minBound ..]
      guard (allowed channel kind)
      either (const Nothing) (\(_, _, p) -> Just p) (runGetOrFail (body kind) (BL.fromStrict (BS.drop n bytes)))

-- | As ngcext checks them.
allowed :: Channel -> Kind -> Bool
allowed Public  = (`elem` [RequestKind, MessageKind, HaveAllKind, AnnounceKind])
allowed Private = (/= MessageKind)

body :: Kind -> Get Packet
body = \case
  RequestKind  -> Request <$> fileKind <*> rest
  InitKind     -> Init <$> fileKind <*> getWord64le <*> getWord8 <*> rest
  Init2Kind    -> Init2 <$> fileKind <*> getWord64le <*> getWord8 <*> getWord8 <*> rest
  InitAckKind  -> initAck <$> getWord8 <*> rest
  DataKind     -> Data <$> getWord8 <*> getWord16le <*> rest
  DataAckKind  -> DataAck <$> getWord8 <*> untilEnd getWord16le
  MessageKind  -> Message <$> getWord32le <*> fileKind <*> rest
  HaveKind     -> Have <$> fileKind <*> sized <*> untilEnd getWord32le
  BitsetKind   -> Bitset <$> fileKind <*> sized <*> getWord32le <*> rest
  HaveAllKind  -> HaveAll <$> fileKind <*> (rest >>= \i -> i <$ guard (not (BS.null i)))
  AnnounceKind -> Announce <$> rest
  where
    fileKind = FileKind <$> getWord32le
    rest = BL.toStrict <$> getRemainingLazyByteString
    sized = getWord16le >>= getByteString . fromIntegral
    untilEnd get = isEmpty >>= \done -> if done then pure [] else (:) <$> get <*> untilEnd get
    -- Each field may be missing from the end, and more may follow.
    initAck i r = case BS.unpack r of
      lo : hi : more -> InitAck i (fromIntegral lo + 256 * fromIntegral hi) (headOr 0 more)
      more           -> InitAck i defaultMaxData (headOr 0 more)
    headOr d = maybe d fst . uncons

encode :: Packet -> BS.ByteString
encode p = BL.toStrict . B.toLazyByteString $ B.word8 (oldId (kindOf p)) <> payload
  where
    fileKind (FileKind k) = B.word32LE k
    sized i = B.word16LE (fromIntegral (BS.length i)) <> B.byteString i
    payload = case p of
      Request k i            -> fileKind k <> B.byteString i
      Init k size t i        -> fileKind k <> B.word64LE size <> B.word8 t <> B.byteString i
      Init2 k size t flags i -> fileKind k <> B.word64LE size <> B.word8 t <> B.word8 flags <> B.byteString i
      -- Without the flags when there are none, as Green-Sky's clients send it.
      InitAck t size flags   -> B.word8 t <> B.word16LE size <> if flags == 0 then mempty else B.word8 flags
      Data t sequ d          -> B.word8 t <> B.word16LE sequ <> B.byteString d
      DataAck t sequs        -> B.word8 t <> foldMap B.word16LE sequs
      Message m k i          -> B.word32LE m <> fileKind k <> B.byteString i
      Have k i chunks        -> fileKind k <> sized i <> foldMap B.word32LE chunks
      Bitset k i first bits  -> fileKind k <> sized i <> B.word32LE first <> B.byteString bits
      HaveAll k i            -> fileKind k <> B.byteString i
      Announce i             -> B.byteString i

lossless :: Packet -> Bool
lossless = (`notElem` [DataKind, DataAckKind]) . kindOf
