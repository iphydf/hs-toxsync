{-# LANGUAGE StrictData #-}
-- | Congestion control for FT1 data as ngcft1 runs it: CUBIC on top of a
-- flow window (cubic.cpp over flow_only.cpp). Sizes are in bytes, times in
-- seconds.
module Tox.Share.Congestion
  ( Congestion
  , Seconds
  , SeqId
  , new
  , rtt
  , window
  , inFlightBytes
  , canSend
  , timeouts
  , sent
  , acked
  , lost
  , congestion
  ) where

import           Data.Foldable (toList)
import           Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import           Data.Word     (Word16, Word8)

type Seconds = Double

-- | A transfer and a sequence number in it.
type SeqId = (Word8, Word16)

data Flying = Flying
  { flyingId    :: SeqId
  , flyingSince :: Seconds
  , flyingBytes :: Int
  , accounted   :: Bool
    -- ^ Counts towards 'inFlightBytes' (timed-out packets stop counting).
  , ignored     :: Bool
    -- ^ Already counted as reordered or lost.
  }

data Congestion = Congestion
  { maxData        :: Int
  , rttAverage     :: Maybe Double
  , flying         :: Seq Flying
  , inFlightBytes  :: Int
  , reorders       :: Staged
  , lastUpdate     :: Seconds
  , windowMax      :: Double
  , sinceReduction :: Double
  }

-- | 100 MiB/s, far above what links carry, so the CUBIC window is what limits.
maxRate :: Double
maxRate = 100 * 1024 * 1024

-- | What a packet costs on the wire besides its data: FT1, Tox, UDP and IPv4.
overhead :: Int
overhead = 4 + 46 + 8 + 20

segment :: Congestion -> Int
segment = (+ overhead) . maxData

-- | Takes the most data each packet may carry.
new :: Seconds -> Int -> Congestion
new now size = Congestion
  { maxData = size
  , rttAverage = Nothing
  , flying = Seq.empty
  , inFlightBytes = 0
  , reorders = noStage
  , lastUpdate = now
  , windowMax = 2 * fromIntegral (size + overhead)
  -- A warm start.
  , sinceReduction = 12
  }

-- | 2 s until we've measured one.
rtt :: Congestion -> Seconds
rtt = maybe 2 (max 0.005 . min 2) . rttAverage

addRtt :: Seconds -> Congestion -> Congestion
addRtt sample c = c { rttAverage = Just (maybe sample weighed (rttAverage c)) }
  where
    weighed old = let alpha = if sample > 3 * old then 0.00002 else 0.0002
                  in alpha * sample + (1 - alpha) * old

flowWindow :: Congestion -> Double
flowWindow c = max (maxRate * rtt c) (2 * fromIntegral (maxData c))

cubicWindow :: Congestion -> Double
cubicWindow c = max (2 * fromIntegral (segment c)) (scale * (sinceReduction c - k) ^ (3 :: Int) + windowMax c)
  where
    -- CUBIC's 0.4 is in packets. ngcft1 uses it on bytes, which grows the
    -- window about a thousand times slower.
    scale = 0.4 * fromIntegral (segment c)
    k = (windowMax c * (1 - 0.8) / scale) ** (1 / 3)

window :: Congestion -> Double
window c = min (cubicWindow c) (flowWindow c)

-- | Shrinks the window, at most once every 4 round trips.
congestion :: Congestion -> Congestion
congestion c
  | sinceReduction c < 4 * rtt c = c
  | otherwise = c
      { sinceReduction = 0
      , windowMax = max (2 * fromIntegral (segment c)) (if cubicWindow c < windowMax c then window c * 0.8 else window c)
      }

-- | How many bytes of data to send now, the last call having been this long
-- ago.
canSend :: Seconds -> Seconds -> Congestion -> (Int, Congestion)
canSend now dt c0 = (if flow == 0 || cubic < segment c2 then 0 else min whole flow, c2)
  where
    (reordered, staged) = stage dt (8 * rtt c0) (reorders c0)
    c1 = (if reordered then congestion else id) c0 { reorders = staged }
    space = floor (flowWindow c1) - inFlightBytes c1
    flowLimited = floor (minimum [fromIntegral space, 1.5 * flowWindow c1 / dt + 0.5, 1.2 * maxRate * dt + 0.5])
    flow
      | space < maxData c1 = 0
      | otherwise = packets (maxData c1) (segment c1) flowLimited
    c2 | not (Seq.null (flying c1)) || now - lastUpdate c1 <= 4 * rtt c1 = c1 { sinceReduction = sinceReduction c1 + dt }
       | otherwise = c1
    cubic = floor (cubicWindow c2) - inFlightBytes c2
    perTick = max (floor (cubicWindow c2 / rtt c2 * dt + 0.5)) (segment c2)
    whole = packets (segment c2) (segment c2) (min cubic perTick)
    -- Whole packets, and a whole segment more for any remainder. ngcft1
    -- means to round up only sometimes, but compares an unsigned random
    -- number with a negative one, so it always does.
    packets size extra n = n `div` size * size + (if n `mod` size == 0 then 0 else extra)

-- | Packets sent more than 6 round trips ago. They stop counting as in
-- flight.
timeouts :: Seconds -> Congestion -> ([SeqId], Congestion)
timeouts now c = (map flyingId (toList late), c { flying = fmap expire (flying c), inFlightBytes = inFlightBytes c - sum (fmap counted late) })
  where
    isLate f = now - 6 * rtt c > flyingSince f
    late = Seq.filter isLate (flying c)
    counted f = if accounted f then flyingBytes f else 0
    expire f = if isLate f then f { accounted = False } else f

-- | A packet went out, with this much data.
sent :: Seconds -> SeqId -> Int -> Congestion -> Congestion
sent now i size c = c
  { flying = flying c |> Flying i now (size + overhead) True False
  , inFlightBytes = inFlightBytes c + size + overhead
  , lastUpdate = now
  }

-- | The first is the packet the ack is for, the rest repeat earlier acks.
-- An ack for a packet that isn't the oldest in flight means one was
-- skipped. Otherwise it measures the round trip.
acked :: Seconds -> [SeqId] -> Congestion -> Congestion
acked _ [] c = c
acked now ids@(primary : _) c0 = foldl remove measured ids
  where
    c = c0 { lastUpdate = now }
    measured = case (Seq.findIndexL ((== primary) . flyingId) (flying c), Seq.findIndexL (not . ignored) (flying c)) of
      (Just i, Just first)
        | not (ignored (Seq.index (flying c) i)) && i /= first ->
            c { flying = Seq.adjust' skip i (Seq.adjust' skip first (flying c)), reorders = addValue 1 (reorders c) }
        | not (ignored (Seq.index (flying c) i)) ->
            addRtt (now - flyingSince (Seq.index (flying c) i)) c { reorders = addValue 0 (reorders c) }
      _ -> c { reorders = addValue 0 (reorders c) }
    skip f = f { ignored = True }
    remove c' i = case Seq.findIndexL ((== i) . flyingId) (flying c') of
      Nothing -> c'
      Just n  -> let f = Seq.index (flying c') n
                 in c' { flying = Seq.deleteAt n (flying c'), inFlightBytes = inFlightBytes c' - (if accounted f then flyingBytes f else 0) }

-- | Forgets a packet that won't be sent again, or restarts the clock on one
-- that will, counting the loss once. 'False' if it wasn't in flight.
lost :: Seconds -> Bool -> SeqId -> Congestion -> (Bool, Congestion)
lost now discard i c = case Seq.findIndexL ((== i) . flyingId) (flying c) of
  Nothing -> (False, c)
  Just n
    | discard -> (True, c { flying = Seq.deleteAt n (flying c), inFlightBytes = inFlightBytes c - (if accounted f then flyingBytes f else 0) })
    | ignored f -> (True, again)
    | otherwise -> (True, congestion again { flying = Seq.adjust' (\g -> g { ignored = True }) n (flying again) })
    where
      f = Seq.index (flying c) n
      again = c { flying = Seq.update n f { flyingSince = now } (flying c) }

-- | Reordered acks over stages of 8 round trips: a stage much worse than
-- the long average means congestion (staged_ema.hpp).
data Staged = Staged
  { average  :: Double
  , recent   :: Double
  , spread   :: Double
  , stageSum :: Double
  , stageDev :: Double
  , count    :: Int
  , elapsed  :: Seconds
  }

noStage :: Staged
noStage = Staged 0 0 0 0 0 0 0

addValue :: Double -> Staged -> Staged
addValue v s = s { stageSum = stageSum s + v, stageDev = stageDev s + abs (average s - v), count = count s + 1 }

-- | Whether the stage that ended shows congestion.
stage :: Seconds -> Seconds -> Staged -> (Bool, Staged)
stage dt duration s
  | count s <= 0 = (False, s)
  | elapsed s + dt < duration = (False, s { elapsed = elapsed s + dt })
  | otherwise = (worse, Staged
      { average = 0.01 * newAverage + 0.99 * average s
      , recent = if confident && not worse then recent' else average s
      , spread = 0.01 * newDev + 0.99 * spread s
      , stageSum = 0, stageDev = 0, count = 0, elapsed = 0
      })
  where
    newAverage = stageSum s / fromIntegral (count s)
    newDev = stageDev s / fromIntegral (count s)
    recent' = 0.25 * newAverage + 0.75 * recent s
    -- Fewer acks than this say too little.
    confident = count s >= 64
    worse = confident && recent' - spread s > (average s + 0.001) * 1.2
