# tiny-scheduler

Utility Library to run jobs concurrently at predertmined Intervals

## Basic Example

```haskell

import TinyScheduler.Jobs
import TinyScheduler.SubJobs
import TinyScheduler.Time
import Data.Time

jobx :: UTCTime -> Job ()
jobx x = Job 1234 x (Secs 20) 4 (putStrLn "Hello")

main :: IO ()
main = getCurrentTime >>= (\x ->
      execSubJobs . convertJobIntoSubJobs x $ (jobx x)) >> 
      return ()

```


# how to install

`stack install tiny-scheduler`


### for a more advanced example (still in progress)
go to  [https://github.com/functor-soup/tiny-simple-scheduler-example](https://github.com/functor-soup/tiny-simple-scheduler-example)
