{- |
  Main Datatype to hold information about a job, and function
  to calculate thread delay
-}
module TinyScheduler.Jobs
  (
  TimeAtom(..),
  makeTimeAtom,
  makeJob
  ) where

import Data.Time
import TinyScheduler.Time

-- | Main datatype atom
data TimeAtom = TimeAtom
  {
  , delay :: UTCTime -> UTCTime -> [Int]
  }

-- | this is the thread delay, calculated as [unit: microseconds)]
-- (startTime - currentTime) + (interval * multiplier)
calculateDelay :: Interval -> Int -> UTCTime -> UTCTime -> [Int]
calculateDelay interval hits startDate currentTime =
  let intervalInSeconds = intervalToSecs interval
      delay = fromEnum $ 10 ^^ (-6) * (diffUTCTime startDate currentTime)
      interval_ = (round (intervalInSeconds * 10 ^ (6))) + delay :: Int
  in map (interval_ *) [0 .. hits]


makeTimeAtom :: Int -> Interval -> TimeAtom
makeTimeAtom x y  = TimeAtom (calculateDelay y z)


instance Monoid TimeAtom where
  mempty = TimeAtom (\x,y -> [])
  mappend x y = let delay = \a,b -> map (delay x a b) >>= \c -> (c+) (delay y a b)
                 in TimeAtom delay

-- | Main datatype to hold job Information
data Job a = Job
  { id :: Int
    delay :: UTCTime -> [Int]
  , job :: IO a
  }

makeJob :: Int -> Int -> Interval -> UTCTime -> IO a -> Job a
makeJob id hits interval startTime job = Job id (calculateDelay hits interval startTime) job
