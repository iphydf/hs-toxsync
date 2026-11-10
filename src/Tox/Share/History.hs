{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
-- | Group history (HS2, spec_ngc_hs2.md): a peer asks for the messages of a
-- time range with an FT1 REQUEST, and gets them back as a file transfer of
-- a MessagePack chatlog, newest first.
module Tox.Share.History
  ( Range (..)
  , Entry (..)
  , Body (..)
  , lastTwoDays
  , encodeRange
  , decodeRange
  , encodeLog
  , decodeLog
  ) where

import           Control.Monad           (guard, when)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as BL
import           Data.Either             (rights)
import           Data.Text               (Text)
import           Data.Word               (Word32, Word64)

import           Tox.Share.Packet        (FileKind (..))
import qualified Tox.Sync.MessagePack    as MP

-- | Seconds since the Unix epoch, the newest first.
data Range = Range
  { newest :: Word64
  , oldest :: Word64
  } deriving (Eq, Ord, Show)

data Entry = Entry
  { time      :: Word64
    -- ^ Deciseconds since the Unix epoch.
  , peer      :: BS.ByteString
    -- ^ The sender's key in the group.
  , messageId :: Word32
  , body      :: Body
  } deriving (Eq, Show)

data Body
  = Text Bool Text
    -- ^ Whether it's an action, and the text.
  | Shared FileKind BS.ByteString
    -- ^ A file shared with FT1_MESSAGE.
  deriving (Eq, Show)

-- | What totato asks each peer for.
lastTwoDays :: Word64 -> Range
lastTwoDays now = Range now (now - 48 * 60 * 60)

-- | The ID of the REQUEST and of the transfer that answers it.
encodeRange :: Range -> BS.ByteString
encodeRange (Range n o) = BL.toStrict (B.toLazyByteString (B.word64LE n <> B.word64LE o))

decodeRange :: BS.ByteString -> Maybe Range
decodeRange bytes = do
  guard (BS.length bytes == 16)
  let r = Range (le (BS.take 8 bytes)) (le (BS.drop 8 bytes))
  r <$ guard (oldest r < newest r)
  where le = BS.foldr (\b n -> fromIntegral b + 256 * n) 0

encodeLog :: [Entry] -> BS.ByteString
encodeLog = MP.encode . MP.array . map entry
  where
    -- Keys in the order nlohmann writes them.
    entry e = MP.object $ case body e of
      Text action text ->
        [("action", MP.bool True) | action] <> [("mid", mid e), ("ppk", MP.bin (peer e)), ("text", MP.str text), ("ts", MP.uint (time e))]
      Shared (FileKind kind) fid ->
        [("fid", MP.bin fid), ("fkind", MP.uint (fromIntegral kind)), ("mid", mid e), ("ppk", MP.bin (peer e)), ("ts", MP.uint (time e))]
    mid = MP.uint . fromIntegral . messageId

-- | Entries that don't read are left out, as totato does.
decodeLog :: BS.ByteString -> Maybe [Entry]
decodeLog = either (const Nothing) (Just . rights . map (MP.decodeExact entry)) . MP.decodeExact (MP.list MP.getRaw)
  where
    entry = do
      n <- MP.getMapLength
      pairs <- mapM (const ((,) <$> MP.getStr <*> MP.getRaw)) [1 .. n]
      let get key g = maybe (fail ("No " <> show key)) (either fail pure . MP.decodeExact g) (lookup key pairs)
          has key = key `elem` map fst pairs
      t <- get "ts" MP.getUint
      p <- get "ppk" MP.getBin
      m <- get "mid" MP.getUint
      when (m > fromIntegral (maxBound :: Word32)) (fail "Message ID out of range")
      b <- if has "text"
        then Text <$> (if has "action" then get "action" MP.getBool else pure False) <*> get "text" MP.getStr
        else Shared <$> (FileKind . fromIntegral <$> get "fkind" MP.getUint) <*> get "fid" MP.getBin
      pure (Entry t p (fromIntegral m) b)
