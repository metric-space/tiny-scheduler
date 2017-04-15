{-| Time Interval Data Type to use to schedule jobs,
   (Secs, Minutes, Hours, Days), coupled along with convenience function
   to convert datatype to a rational number.
   Please note this data type guards against negative time which will result in 0

   Addition/multiplication/division's resultant type instance will determined by the
   left operand

   Sec x  + Day y = Sec z
   Day x + Minute y = Day z

-}
module Time
  ( Interval(..)
  , intervalToSecs
  ) where

import Data.Time

data Interval
  = Secs Rational
  | Minutes Rational
  | Hours Rational
  | Days Rational
  | Weeks Rational
  deriving (Show)

intervalToSecs :: Interval -> Rational
intervalToSecs z =
  case z of
    (Secs x) -> x
    (Minutes x) -> 60 * x
    (Hours x) -> 3600 * x
    (Days x) -> 24 * 3600 * x
    (Weeks x) -> 3600 * 24 * 7 * x

convertToSecs = Secs . intervalToSecs

convertToHours = Hours . flip (/) 3600 . intervalToSecs

convertToMinutes = Minutes . flip (/) 60 . intervalToSecs

convertToDays = Days . flip (/) (24 * 3600) . intervalToSecs

convertToWeeks = Weeks . flip (/) (3600 * 24 * 7) . intervalToSecs

filterOutNegative :: Rational -> Rational
filterOutNegative x = (1 + signum x) * (abs x) / 2

-- to ensure no negative time arrives as a result
guardAgainstZero :: Interval -> Interval
guardAgainstZero y =
  case y of
    (Secs x) -> Secs . filterOutNegative $ x
    (Minutes x) -> Minutes . filterOutNegative $ x
    (Hours x) -> Hours . filterOutNegative $ x
    (Days x) -> Days . filterOutNegative $ x
    (Weeks x) -> Days . filterOutNegative $ x

instance Eq Interval where
  Secs x == Secs y = x == y
  Minutes x == Minutes y = x == y
  Hours x == Hours y = x == y
  Days x == Days y = x == y
  Weeks x == Weeks y = x == y
  x == y = (intervalToSecs x) == (intervalToSecs y)

instance Num Interval where
  Secs x + Secs y = Secs (x + y)
  Secs x + y = Secs x + convertToSecs y
  Minutes x + Minutes y = Minutes (x + y)
  Minutes x + y = Minutes x + convertToMinutes y
  Hours x + Hours y = Hours (x + y)
  Hours x + y = Hours x + convertToHours y
  Days x + Days y = Days (x + y)
  Days x + y = Days x + convertToDays y
  Weeks x + Weeks y = Weeks (x + y)
  Weeks x + y = Weeks x + convertToWeeks y
  Secs x * Secs y = Secs (x * y)
  Secs x * y = Secs x * convertToSecs y
  Minutes x * Minutes y = Minutes (x * y)
  Minutes x * y = Minutes x * convertToMinutes y
  Hours x * Hours y = Hours (x * y)
  Hours x * y = Hours x * convertToHours y
  Days x * Days y = Days (x * y)
  Days x * y = Days x * convertToDays y
  Weeks x * Weeks y = Weeks (x * y)
  Weeks x * y = Weeks x * convertToWeeks y
  -- negation of time makes no sense, this ain't quantum mechanics
  negate _ = Secs 0
  Secs x - Secs y = Secs . filterOutNegative $ (x - y)
  Secs x - y = Secs x - convertToSecs y
  Minutes x - Minutes y = Minutes . filterOutNegative $ (x - y)
  Minutes x - y = Minutes x * convertToMinutes y
  Hours x - Hours y = Hours . filterOutNegative $ (x - y)
  Hours x - y = Hours x - convertToHours y
  Days x - Days y = Days . filterOutNegative $ (x - y)
  Days x - y = Days x - convertToDays y
  Weeks x - Weeks y = Weeks . filterOutNegative $ (x - y)
  Weeks x - y = Weeks x - convertToWeeks y
  abs = guardAgainstZero
  signum = abs
  fromInteger = Secs . fromIntegral
