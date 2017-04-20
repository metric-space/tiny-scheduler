# tiny-scheduler

Utility Library to run jobs concurrently at predetermined Intervals

## Basic Example

```haskell

import TinyScheduler.Jobs
import TinyScheduler.SubJobs
import TinyScheduler.Time
import Data.Time

intervalio :: Interval
intervalio = (Minutes 1) + (Secs 20)

jobx :: UTCTime -> Job ()
jobx x = makeJob 1234 4 intervalio x (putStrLn "Hello")

main :: IO ()
main = getCurrentTime >>= (\x ->
      execSubJobs . convertJobIntoSubJobs x $ (jobx x)) >> 
      return ()

```

### A little more advanced Example

In the following, assuming that time starts at 0, jobs are fired at 1 minute 2 seconds, 1 minutes 4 seconds,
and 2 minutes 2 seconds and 2 minutes 4 seconds from current time

```haskell

import TinyScheduler.Jobs
import TinyScheduler.SubJobs
import TinyScheduler.Time
import Data.Time
import Data.Monoid

atom1 :: TimeAtom
atom1 =  makeTimeAtom 2 (Minutes 1)

atom2 :: TimeAtom
atom2 =  makeTimeAtom 2 (Secs 2)

jobx :: UTCTime -> Job ()
jobx x = timeAtomToJob 1234 (putStrLn "Hello") x (atom1 <> atom2)

main :: IO ()
main = getCurrentTime >>= (\x ->
      execSubJobs . convertJobIntoSubJobs x $ (jobx x)) >> 
      return ()

```

# how to install

`stack install tiny-scheduler`

## pending work
1. Tests
2. More documentation
3. Haddock documentation
4. Complete the example below


### for a more advanced example (still in progress)
go to  [https://github.com/functor-soup/tiny-simple-scheduler-example](https://github.com/functor-soup/tiny-simple-scheduler-example)
