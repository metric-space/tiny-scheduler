{- |
  Main Datatype to hold information about a job, and function
  to calculate thread delay
-}
module TinyScheduler.Jobs
  ( TimeAtom(..)
  , Job(..)
  , makeTimeAtom
  , makeJob
  ) where

import Data.Time
import Prelude hiding (id)
import TinyScheduler.Time

-- | Main datatype atom
data TimeAtom = TimeAtom
  { delay_ :: UTCTime -> UTCTime -> [Int]
  }

-- | this is the thread delay, calculated as [unit: microseconds)]
-- (startTime - currentTime) + (interval * multiplier)
calculateDelay :: Interval -> Int -> UTCTime -> UTCTime -> [Int]
calculateDelay interval hits startDate currentTime =
  let intervalInSeconds = intervalToSecs interval
      delay = fromEnum $ 10 ^^ (-6) * (diffUTCTime startDate currentTime)
      interval_ = (round (intervalInSeconds * 10 ^ (6))) + delay :: Int
  in map (interval_ *) [1 .. hits]

makeTimeAtom :: Int -> Interval -> TimeAtom
makeTimeAtom x y = TimeAtom (calculateDelay y x)

instance Monoid TimeAtom where
  mempty = TimeAtom (\x y -> [])
  mappend x y =
    let delay = \a b -> ((delay_ x a b) >>= \c -> map (c +) (delay_ y a b))
    in TimeAtom delay

-- | Main datatype to hold job Information
data Job a = Job
  { id :: Int
  , delay :: UTCTime -> [Int]
  , job :: IO a
  }

timeAtomToJob :: Int -> IO a -> UTCTime -> TimeAtom -> Job a
timeAtomToJob id job start atom = Job {id = id, delay = (delay_ atom start), job = job}

makeJob :: Int -> Int -> Interval -> UTCTime -> IO a -> Job a
makeJob id hits interval startTime job = Job id (calculateDelay interval hits startTime) job
