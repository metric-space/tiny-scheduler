{- |
  Main function that calculates thread delay associated
  with each job
-}
module TinyScheduler.Utils
  ( calculateDelay
  ) where

import Data.Time
import TinyScheduler.Time

-- | this is the thread delay, calculated as [unit: microseconds)]
-- (startTime - currentTime) + (interval * multiplier)
calculateDelay :: Interval -> Int -> UTCTime -> UTCTime -> [Int]
calculateDelay interval hits startDate currentTime =
  let intervalInSeconds = intervalToSecs interval
      delay = fromEnum $ 10 ^^ (-6) * (diffUTCTime startDate currentTime)
      interval_ = (round (intervalInSeconds * 10 ^ (6))) + delay :: Int
  in map (interval_ *) [1 .. hits]
