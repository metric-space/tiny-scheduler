{- |
  Main Datatype to hold information about a job, and function
  to calculate thread delay
-}
module Jobs
  ( Job(..)
  , calculateDelay
  ) where

import Control.Concurrent
import Data.Time
import Time

-- | Main datatype to hold job Information
data Job a = Job
  { id :: Int
  , startDate :: UTCTime
  , interval :: Interval
  , hits :: Int
  , job :: IO a
  }

-- | this is the thread delay, calculated as [unit: microseconds)]
-- (startTime - currentTime) + (interval * multiplier)
calculateDelay :: UTCTime -> UTCTime -> Interval -> Int -> [Int]
calculateDelay currentTime startDate interval hits =
  let intervalInSeconds = intervalToSecs interval
      delay = fromEnum $ 10 ^^ (-6) * (diffUTCTime startDate currentTime)
      interval_ = (round (intervalInSeconds * 10 ^ (6))) + delay :: Int
  in map (interval_ *) [0 .. hits]
