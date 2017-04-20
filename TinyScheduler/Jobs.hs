{- |
  Main Datatype to hold information about a job, and function
  to calculate thread delay
-}
module TinyScheduler.Jobs
  ( Job(..)
  , makeJob
  , timeAtomToJob
  ) where

import Data.Time
import Prelude hiding (id)
import TinyScheduler.Time
import TinyScheduler.TimeAtom
import TinyScheduler.Utils (calculateDelay)

-- | Main datatype to hold job Information
data Job a = Job
  { id :: Int
  , delay :: UTCTime -> [Int]
  , job :: IO a
  }

-- | Convert time atom to Job
timeAtomToJob :: Int -> IO a -> UTCTime -> TimeAtom -> Job a
timeAtomToJob id job start atom = Job {id = id, delay = (delay_ atom start), job = job}

-- | Function to generate job from information about id, no of hits, interval
-- | startTine and the a function with side effects
makeJob :: Int -> Int -> Interval -> UTCTime -> IO a -> Job a
makeJob id hits interval startTime job = Job id (calculateDelay interval hits startTime) job
