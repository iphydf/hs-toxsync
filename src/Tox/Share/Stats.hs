{-# LANGUAGE StrictData #-}
-- | How fast and how much of each file goes each way, as
-- transfer_stats_systems.cpp tallies it.
module Tox.Share.Stats
  ( Tally
  , Stats (..)
  , empty
  , sent
  , received
  , stats
  ) where

import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)
import           Data.Word       (Word64)

import           Tox.Share.Swarm (FileId)

type Seconds = Double

newtype Tally = Tally (Map.Map FileId Way)

-- | Both ways of one file.
data Way = Way
  { up   :: Direction
  , down :: Direction
  }

data Direction = Direction
  { recent :: [(Seconds, Int)]
    -- ^ Bytes in the last second, newest first.
  , total  :: Word64
  }

data Stats = Stats
  { rateUp    :: Double
    -- ^ Bytes a second, over the last second.
  , rateDown  :: Double
  , totalUp   :: Word64
  , totalDown :: Word64
  }
  deriving (Eq, Show)

empty :: Tally
empty = Tally Map.empty

-- | Bytes of the file we sent, resent ones too.
sent :: Seconds -> FileId -> Int -> Tally -> Tally
sent now fid n = onFile fid (\w -> w { up = add now n (up w) })

received :: Seconds -> FileId -> Int -> Tally -> Tally
received now fid n = onFile fid (\w -> w { down = add now n (down w) })

onFile :: FileId -> (Way -> Way) -> Tally -> Tally
onFile fid f (Tally t) = Tally (Map.alter (Just . f . fromMaybe (Way none none)) fid t)
  where none = Direction [] 0

add :: Seconds -> Int -> Direction -> Direction
add now n d = Direction ((now, n) : lastSecond now (recent d)) (total d + fromIntegral n)

lastSecond :: Seconds -> [(Seconds, Int)] -> [(Seconds, Int)]
lastSecond now = takeWhile ((> now - 1) . fst)

stats :: Seconds -> FileId -> Tally -> Stats
stats now fid (Tally t) = maybe (Stats 0 0 0 0) (\w -> Stats (rate (up w)) (rate (down w)) (total (up w)) (total (down w))) (Map.lookup fid t)
  where
    rate = fromIntegral . sum . map snd . lastSecond now . recent
