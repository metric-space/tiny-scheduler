module TinyScheduler.SubJobs
  ( convertJobIntoSubJobs
  , execSubJobs
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Data.Time
import Prelude hiding (id)
import TinyScheduler.Jobs

data SubJob a = SubJob
  { jobId :: Int
  , subJobNo :: Int
  , delayx :: Int
  , job_ :: IO a
  }

convertJobIntoSubJobs :: UTCTime -> Job a -> [SubJob a]
convertJobIntoSubJobs currentTime x =
  let timeDelays = (delay x currentTime)
      zippedDelays = zip [1 ..] timeDelays
   in map (\(i, z) -> SubJob (id x) i z (job x)) zippedDelays

execSubJob :: SubJob a -> IO a
execSubJob x = threadDelay (delayx x) >> (job_ x)

execSubJobs :: [SubJob a] -> IO [a]
execSubJobs = mapConcurrently execSubJob
