{-| Module for time atoms  -}
module TinyScheduler.TimeAtom
  ( TimeAtom(..)
  , makeTimeAtom
  ) where

import Data.Time
import TinyScheduler.Time
import TinyScheduler.Utils (calculateDelay)

-- | Composable Time atom
data TimeAtom = TimeAtom
  { delay_ :: UTCTime -> UTCTime -> [Int]
  }

-- | Exposed function to create time atoms
makeTimeAtom :: Int -> Interval -> TimeAtom
makeTimeAtom x y = TimeAtom (calculateDelay y x)

-- | The composability sauce
--  if you have two time atoms that when visualized look
--  somewhat like shown below
--  ......(a).............(a).............
--           <>
--  ..(b)..(b)............................
--             =
-- ..........(b)..(b)..........(b)..(b)...
instance Monoid TimeAtom where
  mempty = TimeAtom (\x y -> [0])
instance Semigroup TimeAtom where
   x <> y =
    let delay = \a b -> ((delay_ x a b) >>= \c -> map (c +) (delay_ y a b))
    in TimeAtom delay
