{-# LANGUAGE StrictData #-}
-- | What a file shared in the SHA1 swarm is: its name, size and the SHA1 of
-- each chunk, laid out as ft1_sha1_info.cpp does. Its own SHA1 is the
-- file's ID.
module Tox.Share.Info
  ( Info (..)
  , decode
  , encode
  , hash
  , sha1
  , chunkSizeFor
  , chunkLength
  , chunkCount
  , chunkHash
  , chunks
  ) where

import           Control.Monad           (guard)
import qualified Crypto.Hash.SHA1        as SHA1
import           Data.Bits               (countLeadingZeros, shiftL)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as BL
import           Data.Word               (Word32, Word64)

data Info = Info
  { name      :: BS.ByteString
  , size      :: Word64
  , chunkSize :: Word32
  , hashes    :: BS.ByteString
    -- ^ The chunks' SHA1s back to back, as sent. A peer can send up to
    -- 'Tox.Share.Swarm.maxInfo' of them, so they stay in one string.
  } deriving (Eq, Show)

sha1 :: BS.ByteString -> BS.ByteString
sha1 = SHA1.hash

-- | The file's ID.
hash :: Info -> BS.ByteString
hash = sha1 . encode

-- | The name takes 256 bytes: cut, or padded with zeros.
encode :: Info -> BS.ByteString
encode i = BL.toStrict . B.toLazyByteString $ B.byteString padded <> B.word64LE (size i) <> B.word32LE (chunkSize i) <> B.byteString (hashes i)
  where padded = BS.take 256 (name i <> BS.replicate 256 0)

-- | 'Nothing' unless the chunks cover the size exactly, so that every
-- chunk's length is known.
decode :: BS.ByteString -> Maybe Info
decode bytes = do
  guard (BS.length bytes >= 268 && (BS.length bytes - 268) `mod` 20 == 0)
  let i = Info (BS.takeWhile (/= 0) (BS.take 256 bytes)) (le (BS.take 8 (BS.drop 256 bytes))) (le (BS.take 4 (BS.drop 264 bytes))) (BS.drop 268 bytes)
      count = fromIntegral (chunkCount i) :: Word64
  guard (chunkSize i > 0 && size i > 0 && count == (size i + fromIntegral (chunkSize i) - 1) `div` fromIntegral (chunkSize i))
  pure i
  where
    le :: Num a => BS.ByteString -> a
    le = BS.foldr (\b n -> fromIntegral b + 256 * n) 0

-- | 32 KiB up to 512 KiB, 4 MiB from 2 GiB, and between them the power of
-- two above a straight line, as ngcft1 picks it.
chunkSizeFor :: Word64 -> Word32
chunkSizeFor fileSize
  | fileSize <= low = 32 * 1024
  | fileSize >= high = 4 * 1024 * 1024
  | otherwise = nextPowerOfTwo (floor ((1 - t) * 32 * 1024 + t * 4 * 1024 * 1024))
  where
    low = 512 * 1024
    high = 2 * 1024 * 1024 * 1024
    t = fromIntegral (fileSize - low) / fromIntegral high :: Double
    nextPowerOfTwo :: Word64 -> Word32
    nextPowerOfTwo x = fromIntegral (1 `shiftL` (64 - countLeadingZeros (x - 1)) :: Word64)

-- | The last chunk is what's left.
chunkLength :: Info -> Int -> Int
chunkLength i n
  | n + 1 == chunkCount i = fromIntegral (size i - fromIntegral n * fromIntegral (chunkSize i))
  | otherwise = fromIntegral (chunkSize i)

chunkCount :: Info -> Int
chunkCount = (`div` 20) . BS.length . hashes

chunkHash :: Info -> Int -> Maybe BS.ByteString
chunkHash i n
  | n >= 0 && n < chunkCount i = Just (BS.take 20 (BS.drop (20 * n) (hashes i)))
  | otherwise = Nothing

chunks :: Info -> [BS.ByteString]
chunks i = [BS.take 20 (BS.drop (20 * n) (hashes i)) | n <- [0 .. chunkCount i - 1]]
