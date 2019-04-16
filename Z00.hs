{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Z00 where

import Control.Applicative((<*>))
import Control.Monad((>=>))
import Data.Bool(bool)

-- $setup
-- >>> import Data.Maybe(isNothing)

-- Five x ~ x ^ 5
data FiveOf x =
  FiveOf x x x x x
  deriving (Eq, Show)

-- 1 + 1 + 1 + 1 + 1 = 5
data UpToFive =
  One | Two | Three | Four | Five
  deriving (Eq, Show)

-- | Add 1 to `UpToFive`. Returns `Nothing` if passed `Five`.
add1 ::
  UpToFive
  -> Maybe UpToFive
add1 One =
  Just Two
add1 Two =
  Just Three
add1 Three =
  Just Four
add1 Four =
  Just Five
add1 Five =
  Nothing

-- | Subtract 1 from `UpToFive`. Returns `Nothing` if passed `One`.
subtract1 ::
  UpToFive
  -> Maybe UpToFive
subtract1 One =
  Nothing
subtract1 Two =
  Just One
subtract1 Three =
  Just Two
subtract1 Four =
  Just Three
subtract1 Five =
  Just Four

-- | Add 1 to `UpToFive`. Returns `One` if passed `Five`.
add1Cycle ::
  UpToFive
  -> UpToFive
add1Cycle One =
  Two
add1Cycle Two =
  Three
add1Cycle Three =
  Four
add1Cycle Four =
  Five
add1Cycle Five =
  One

-- | Subtract 1 from `UpToFive`. Returns `Five` if passed `One`.
subtract1Cycle ::
  UpToFive
  -> UpToFive
subtract1Cycle One =
  Five
subtract1Cycle Two =
  One
subtract1Cycle Three =
  Two
subtract1Cycle Four =
  Three
subtract1Cycle Five =
  Four
  
-- d/dx. Five x
-- FiveOfDerivative x ~ 5 * x * x * x * x
data FiveOfDerivative x =
  FiveOfDerivative
    UpToFive
    x
    x
    x
    x
  deriving (Eq, Show)

data FiveOfZipper x =
  FiveOfZipper
    x -- 1-hole
    (FiveOfDerivative x)
  deriving (Eq, Show)

instance Functor FiveOf where
  fmap f (FiveOf x1 x2 x3 x4 x5) =
    FiveOf (f x1) (f x2) (f x3) (f x4) (f x5)

instance Functor FiveOfDerivative where
  fmap f (FiveOfDerivative thr x1 x2 x3 x4) =
    FiveOfDerivative thr (f x1) (f x2) (f x3) (f x4)

instance Functor FiveOfZipper where
  fmap f (FiveOfZipper focus dx) =
    FiveOfZipper (f focus) (fmap f dx)

-- | Create a zipper for five values, with focus on the first value.
--
-- >>> toFiveOfZipper (FiveOf "a" "b" "c" "d" "e")
-- FiveOfZipper "a" (FiveOfDerivative One "b" "c" "d" "e")
toFiveOfZipper ::
  FiveOf x
  -> FiveOfZipper x
toFiveOfZipper (FiveOf x1 x2 x3 x4 x5) =
  FiveOfZipper x1 (FiveOfDerivative One x2 x3 x4 x5)

-- | Create five values from a zipper.
--
-- λ> fromFiveOfZipper (FiveOfZipper "a" (FiveOfDerivative Two "b" "c" "d" "e"))
-- FiveOf "a" "b" "c" "d" "e"
--
-- >>> fromFiveOfZipper (FiveOfZipper "a" (FiveOfDerivative One "b" "c" "d" "e"))
-- FiveOf "a" "b" "c" "d" "e"
--
-- >>> fromFiveOfZipper (FiveOfZipper "a" (FiveOfDerivative Five "b" "c" "d" "e"))
-- FiveOf "a" "b" "c" "d" "e"
fromFiveOfZipper ::
  FiveOfZipper x
  -> FiveOf x
fromFiveOfZipper (FiveOfZipper x1 (FiveOfDerivative _ x2 x3 x4 x5)) =
  FiveOf x1 x2 x3 x4 x5

-- | Move the zipper focus one position to the right.
--
-- If the zipper focus is already at the right-most position, return `Nothing`.
--
-- /Tip/ Use `add1`
--
-- >>> moveRight (FiveOfZipper "a" (FiveOfDerivative One "b" "c" "d" "e"))
-- Just (FiveOfZipper "a" (FiveOfDerivative Two "b" "c" "d" "e"))
--
-- >>> moveRight (FiveOfZipper "a" (FiveOfDerivative Two "b" "c" "d" "e"))
-- Just (FiveOfZipper "a" (FiveOfDerivative Three "b" "c" "d" "e"))
--
-- >>> moveRight (FiveOfZipper "a" (FiveOfDerivative Four "b" "c" "d" "e"))
-- Just (FiveOfZipper "a" (FiveOfDerivative Five "b" "c" "d" "e"))
--
-- >>> moveRight (FiveOfZipper "a" (FiveOfDerivative Five "b" "c" "d" "e"))
-- Nothing
moveRight ::
  FiveOfZipper x
  -> Maybe (FiveOfZipper x)
moveRight (FiveOfZipper x1 (FiveOfDerivative pos x2 x3 x4 x5)) =
  fmap (\p -> FiveOfZipper x1 (FiveOfDerivative p x2 x3 x4 x5)) (add1 pos)

-- | Move the zipper focus one position to the left.
--
-- If the zipper focus is already at the left-most position, return `Nothing`.
--
-- /Tip/ Use `subtract1`
--
-- >>> moveLeft (FiveOfZipper "a" (FiveOfDerivative Five "b" "c" "d" "e"))
-- Just (FiveOfZipper "a" (FiveOfDerivative Four "b" "c" "d" "e"))
--
-- >>> moveLeft (FiveOfZipper "a" (FiveOfDerivative Four "b" "c" "d" "e"))
-- Just (FiveOfZipper "a" (FiveOfDerivative Three "b" "c" "d" "e"))
--
-- >>> moveLeft (FiveOfZipper "a" (FiveOfDerivative Two "b" "c" "d" "e"))
-- Just (FiveOfZipper "a" (FiveOfDerivative One "b" "c" "d" "e"))
--
-- >>> moveLeft (FiveOfZipper "a" (FiveOfDerivative One "b" "c" "d" "e"))
-- Nothing
moveLeft ::
  FiveOfZipper x
  -> Maybe (FiveOfZipper x)
moveLeft (FiveOfZipper x1 (FiveOfDerivative pos x2 x3 x4 x5)) =
  fmap (\p -> FiveOfZipper x1 (FiveOfDerivative p x2 x3 x4 x5)) (subtract1 pos)

-- | Move the zipper focus one position to the right.
--
-- If the zipper focus is already at the right-most position, cycle the zipper
-- to the start position.
--
-- /Tip/ Use `add1Cycle`
--
-- >>> moveRightCycle (FiveOfZipper "a" (FiveOfDerivative One "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative Two "b" "c" "d" "e")
--
-- >>> moveRightCycle (FiveOfZipper "a" (FiveOfDerivative Two "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative Three "b" "c" "d" "e")
--
-- >>> moveRightCycle (FiveOfZipper "a" (FiveOfDerivative Three "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative Four "b" "c" "d" "e")
--
-- >>> moveRightCycle (FiveOfZipper "a" (FiveOfDerivative Four "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative Five "b" "c" "d" "e")
--
-- >>> moveRightCycle (FiveOfZipper "a" (FiveOfDerivative Five "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative One "b" "c" "d" "e")
moveRightCycle ::
  FiveOfZipper x
  -> FiveOfZipper x
moveRightCycle (FiveOfZipper x1 (FiveOfDerivative p x2 x3 x4 x5)) =
  FiveOfZipper x1 (FiveOfDerivative (add1Cycle p) x2 x3 x4 x5)

-- | Move the zipper focus one position to the left.
--
-- If the zipper focus is already at the left-most position, cycle the zipper
-- to the start position.
--
-- /Tip/ Use `subtract1Cycle`
--
-- >>> moveLeftCycle (FiveOfZipper "a" (FiveOfDerivative Five "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative Four "b" "c" "d" "e")
--
-- >>> moveLeftCycle (FiveOfZipper "a" (FiveOfDerivative Four "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative Three "b" "c" "d" "e")
--
-- >>> moveLeftCycle (FiveOfZipper "a" (FiveOfDerivative Two "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative One "b" "c" "d" "e")
--
-- >>> moveLeftCycle (FiveOfZipper "a" (FiveOfDerivative One "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative Five "b" "c" "d" "e")
moveLeftCycle ::
  FiveOfZipper x
  -> FiveOfZipper x
moveLeftCycle (FiveOfZipper x1 (FiveOfDerivative p x2 x3 x4 x5)) =
  FiveOfZipper x1 (FiveOfDerivative (subtract1Cycle p) x2 x3 x4 x5)

-- | Move the zipper focus the given number of positions
--   * to the left if negative
--   * to the right if positive
--
-- /Tip/ Use `moveLeft` and `moveRight`
-- 
-- If the zipper focus moves off the right-most or left-most position, return `Nothing`.
move ::
  Int
  -> FiveOfZipper x
  -> Maybe (FiveOfZipper x)
move n =
  let n' = abs n
      k = if n < 0 then moveLeft else moveRight
  in  foldr (>=>) pure . replicate n' $ k

-- | Move the zipper focus the given number of positions
--   * to the left if negative
--   * to the right if positive
--
-- /Tip/ Use `moveLeftCycle` and `moveRightCycle`
-- /Tip/ Use `mod` to improve efficiency
-- 
-- If the zipper focus moves off the right-most cycle to the left-most position.
-- If the zipper focus moves off the left-most cycle to the right-most position.
--
-- >>> moveCycle 0 (FiveOfZipper "a" (FiveOfDerivative One "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative One "b" "c" "d" "e")
--
-- >>> moveCycle 0 (FiveOfZipper "a" (FiveOfDerivative Two "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative Two "b" "c" "d" "e")
--
-- >>> moveCycle 0 (FiveOfZipper "a" (FiveOfDerivative Five "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative Five "b" "c" "d" "e")
--
-- >>> moveCycle 1 (FiveOfZipper "a" (FiveOfDerivative Two "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative Three "b" "c" "d" "e")
--
-- >>> moveCycle 1 (FiveOfZipper "a" (FiveOfDerivative One "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative Two "b" "c" "d" "e")
--
-- >>> moveCycle 1 (FiveOfZipper "a" (FiveOfDerivative Two "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative Three "b" "c" "d" "e")
--
-- >>> moveCycle 1 (FiveOfZipper "a" (FiveOfDerivative Five "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative One "b" "c" "d" "e")
--
-- >>> moveCycle 3 (FiveOfZipper "a" (FiveOfDerivative One "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative Four "b" "c" "d" "e")
--
-- >>> moveCycle 3 (FiveOfZipper "a" (FiveOfDerivative Three "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative One "b" "c" "d" "e")
--
-- >>> moveCycle (-1) (FiveOfZipper "a" (FiveOfDerivative Five "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative One "b" "c" "d" "e")
--
-- >>> moveCycle (-1) (FiveOfZipper "a" (FiveOfDerivative Three "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative Four "b" "c" "d" "e")
--
-- >>> moveCycle (-1) (FiveOfZipper "a" (FiveOfDerivative One "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative Two "b" "c" "d" "e")
--
-- >>> moveCycle (-3) (FiveOfZipper "a" (FiveOfDerivative Five "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative Three "b" "c" "d" "e")
--
-- >>> moveCycle (-3) (FiveOfZipper "a" (FiveOfDerivative Four "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative Two "b" "c" "d" "e")
--
-- >>> moveCycle (-3) (FiveOfZipper "a" (FiveOfDerivative Two "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative Five "b" "c" "d" "e")
--
-- >>> moveCycle 15 (FiveOfZipper "a" (FiveOfDerivative One "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative One "b" "c" "d" "e")
--
-- >>> moveCycle 16 (FiveOfZipper "a" (FiveOfDerivative One "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative Two "b" "c" "d" "e")
--
-- >>> moveCycle 19 (FiveOfZipper "a" (FiveOfDerivative One "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative Five "b" "c" "d" "e")
--
-- >>> moveCycle (-15) (FiveOfZipper "a" (FiveOfDerivative One "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative One "b" "c" "d" "e")
--
-- >>> moveCycle (-16) (FiveOfZipper "a" (FiveOfDerivative One "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative Two "b" "c" "d" "e")
--
-- >>> moveCycle (-19) (FiveOfZipper "a" (FiveOfDerivative One "b" "c" "d" "e"))
-- FiveOfZipper "a" (FiveOfDerivative Five "b" "c" "d" "e")
moveCycle ::
  Int
  -> FiveOfZipper x
  -> FiveOfZipper x
moveCycle n =
  let n' = abs (n `mod` 5)
      k = if n < 0 then moveLeftCycle else moveRightCycle
  in  foldr (.) id . replicate n' $ k

-- | Modify the zipper focus using the given function.
--
-- >>> modifyFocus (+1) (FiveOfZipper 10 (FiveOfDerivative One 11 12 13 14))
-- FiveOfZipper 11 (FiveOfDerivative One 11 12 13 14)
--
-- >>> modifyFocus (+1) (FiveOfZipper 10 (FiveOfDerivative Two 11 12 13 14))
-- FiveOfZipper 10 (FiveOfDerivative Two 12 12 13 14)
--
-- >>> modifyFocus (+1) (FiveOfZipper 10 (FiveOfDerivative Five 11 12 13 14))
-- FiveOfZipper 10 (FiveOfDerivative Five 11 12 13 15)
modifyFocus ::
  (x -> x)
  -> FiveOfZipper x
  -> FiveOfZipper x
modifyFocus k (FiveOfZipper x1 (FiveOfDerivative pos x2 x3 x4 x5)) =
  let (k1, k2, k3, k4, k5) =
        case pos of
          One ->
            (k, id, id, id, id)
          Two ->
            (id, k, id, id, id)
          Three ->
            (id, id, k, id, id)
          Four ->
            (id, id, id, k, id)
          Five ->
            (id, id, id, id, k)
  in  FiveOfZipper (k1 x1) (FiveOfDerivative pos (k2 x2) (k3 x3) (k4 x4) (k5 x5))

-- | Set the zipper focus to the given value.
--
-- /Tip/ Use `modifyFocus`
--
-- >>> setFocus 99 (FiveOfZipper 10 (FiveOfDerivative One 11 12 13 14))
-- FiveOfZipper 99 (FiveOfDerivative One 11 12 13 14)
--
-- >>> setFocus 99 (FiveOfZipper 10 (FiveOfDerivative Two 11 12 13 14))
-- FiveOfZipper 10 (FiveOfDerivative Two 99 12 13 14)
--
-- >>> setFocus 99 (FiveOfZipper 10 (FiveOfDerivative Five 11 12 13 14))
-- FiveOfZipper 10 (FiveOfDerivative Five 11 12 13 99)
setFocus ::
  x
  -> FiveOfZipper x
  -> FiveOfZipper x
setFocus =
  modifyFocus . const

-- | Return the zipper focus.
--
-- >>> getFocus (FiveOfZipper 10 (FiveOfDerivative One 11 12 13 14))
-- 10
--
-- >>> getFocus (FiveOfZipper 10 (FiveOfDerivative Three 11 12 13 14))
-- 12
--
-- >>> getFocus (FiveOfZipper 10 (FiveOfDerivative Five 11 12 13 14))
-- 14
getFocus ::
  FiveOfZipper x
  -> x
getFocus (FiveOfZipper x1 (FiveOfDerivative pos x2 x3 x4 x5)) =
  case pos of
    One ->
      x1
    Two ->
      x2
    Three ->
      x3
    Four ->
      x4
    Five ->
      x5

-- | Duplicate a zipper of zippers, from the given zipper.
--
-- >>> duplicate (FiveOfZipper 10 (FiveOfDerivative Five 11 12 13 14))
-- FiveOfZipper (FiveOfZipper 10 (FiveOfDerivative One 11 12 13 14)) (FiveOfDerivative Five (FiveOfZipper 10 (FiveOfDerivative Two 11 12 13 14)) (FiveOfZipper 10 (FiveOfDerivative Three 11 12 13 14)) (FiveOfZipper 10 (FiveOfDerivative Four 11 12 13 14)) (FiveOfZipper 10 (FiveOfDerivative Five 11 12 13 14)))
duplicate ::
  FiveOfZipper x
  -> FiveOfZipper (FiveOfZipper x)
duplicate (FiveOfZipper x1 (FiveOfDerivative pos x2 x3 x4 x5)) =
  let z' pos' =
        FiveOfZipper x1 (FiveOfDerivative pos' x2 x3 x4 x5)
  in  FiveOfZipper (z' One) (FiveOfDerivative pos (z' Two) (z' Three) (z' Four) (z' Five))

-- | This is a test of `getFocus` and `duplicate` that should always return `Nothing`.
-- If the test fails, two unequal values (which should be equal) are returned in `Just`.
--
-- >>> law1 (FiveOfZipper 10 (FiveOfDerivative Five 11 12 13 14))
-- Nothing
law1 ::
  Eq x =>
  FiveOfZipper x
  -> Maybe (FiveOfZipper x, FiveOfZipper x)
law1 x =
  let x' = getFocus (duplicate x)
  in  if x == x'
        then
          Nothing
        else
          Just (x, x')

-- | This is a test of `getFocus` and `duplicate` that should always return `Nothing`.
-- If the test fails, two unequal values (which should be equal) are returned in `Just`.
--
-- >>> law2 (FiveOfZipper 10 (FiveOfDerivative Five 11 12 13 14))
-- Nothing
law2 ::
  Eq x =>
  FiveOfZipper x
  -> Maybe (FiveOfZipper x, FiveOfZipper x)
law2 x =
  let x' = fmap getFocus (duplicate x)
  in  if x == x'
        then
          Nothing
        else
          Just (x, x')

-- | This is a test of `duplicate` that should always return `Nothing`.
-- If the test fails, two unequal values (which should be equal) are returned in `Just`.
--
-- >>> law3 (FiveOfZipper 10 (FiveOfDerivative Five 11 12 13 14))
-- Nothing
law3 ::
  Eq x =>
  FiveOfZipper x
  -> Maybe (FiveOfZipper x, FiveOfZipper (FiveOfZipper (FiveOfZipper x)), FiveOfZipper (FiveOfZipper (FiveOfZipper x)))
law3 x =
  let x' = duplicate (duplicate x)
      x'' = fmap duplicate (duplicate x)
  in  if x' == x''
        then
          Nothing
        else
          Just (x, x', x'')

-- | Used to implement `findRight` and `findLeft`.
satisfy ::
  Monad m =>
  (x -> m x)
  -> (x -> Bool)
  -> x
  -> m x
satisfy k p z =
  k z >>= bool <$> satisfy k p <*> pure <*> p

-- | Move the zipper focus right until the focus satisfies the given predicate.
--
-- /Tip/ Use `satisfy` and `moveRight`
--
-- >>> findRight even (FiveOfZipper 10 (FiveOfDerivative One 11 12 13 14))
-- Just (FiveOfZipper 10 (FiveOfDerivative Three 11 12 13 14))
--
-- >>> findRight even (FiveOfZipper 10 (FiveOfDerivative Two 11 12 13 14))
-- Just (FiveOfZipper 10 (FiveOfDerivative Three 11 12 13 14))
--
-- >>> findRight even (FiveOfZipper 10 (FiveOfDerivative Three 11 12 13 14))
-- Just (FiveOfZipper 10 (FiveOfDerivative Five 11 12 13 14))
--
-- >>> findRight even (FiveOfZipper 10 (FiveOfDerivative Five 11 12 13 14))
-- Nothing
--
-- >>> findRight even (FiveOfZipper 10 (FiveOfDerivative One 11 13 15 17))
-- Nothing
findRight ::
  (x -> Bool)
  -> FiveOfZipper x
  -> Maybe (FiveOfZipper x)
findRight p =
  satisfy moveRight (p . getFocus)

-- | Move the zipper focus left until the focus satisfies the given predicate.
--
-- /Tip/ Use `satisfy` and `moveLeft`
--
-- >>> findLeft even (FiveOfZipper 10 (FiveOfDerivative Five 11 12 13 14))
-- Just (FiveOfZipper 10 (FiveOfDerivative Three 11 12 13 14))
--
-- >>> findLeft even (FiveOfZipper 10 (FiveOfDerivative Four 11 12 13 14))
-- Just (FiveOfZipper 10 (FiveOfDerivative Three 11 12 13 14))
--
-- >>> findLeft even (FiveOfZipper 10 (FiveOfDerivative Three 11 12 13 14))
-- Just (FiveOfZipper 10 (FiveOfDerivative One 11 12 13 14))
--
-- >>> findLeft even (FiveOfZipper 10 (FiveOfDerivative One 11 12 13 14))
-- Nothing
--
-- >>> findLeft even (FiveOfZipper 7 (FiveOfDerivative Five 9 11 13 14))
-- Nothing
findLeft ::
  (x -> Bool)
  -> FiveOfZipper x
  -> Maybe (FiveOfZipper x)
findLeft p =
  satisfy moveLeft (p . getFocus)

-- | If the zipper focus satisfies the given predicate, return the given zipper.
-- Otherwise, move the zipper focus left until the focus satisfies the given predicate.
-- This may be the thought of as `findRight` but the zipper may not move, if the focus satisfies the predicate.
findRightIncl ::
  (x -> Bool)
  -> FiveOfZipper x
  -> Maybe (FiveOfZipper x)
findRightIncl p z =
  bool (findRight p z) (Just z) (p (getFocus z))

-- | Given 5 numbers, find the first number that is even,
-- then on the number previous to that, add 1.
-- Returns `Nothing` if the first number is even,
-- or there are no even numbers.
--
-- /Tip/ Use `findRightIncl` and `add1Focus` and `moveLeft`
--
-- >>> example1 (FiveOf 11 33 55 66 99)
-- Just (FiveOf 11 33 56 66 99)
--
-- >>> example1 (FiveOf 22 33 55 66 99)
-- Nothing
--
-- >>> example1 (FiveOf 11 33 55 77 99)
-- Nothing
example1 ::
  FiveOf Integer
  -> Maybe (FiveOf Integer)
example1 is =
  let firstEven =
        findRightIncl even (toFiveOfZipper is)
      add1Focus =
        fromFiveOfZipper . modifyFocus (+1)
  in  fmap add1Focus (firstEven >>= moveLeft)

-- | Given 5 numbers, find the first multiple of 7,
-- then modulo that number with 5 and move the zipper to that (position+1) and
-- return the focus.
-- Returns `Nothing` if there is no multiple of 7.
--
-- /Tip/ Use `findRightIncl` and `moveCycle`
--
-- >>> example2 (FiveOf 22 33 44 77 99)
-- Just 22
--
-- >>> example2 (FiveOf 14 33 44 77 99)
-- Just 99
--
-- >>> example2 (FiveOf 35 33 44 77 99)
-- Just 35
--
-- >>> example2 (FiveOf 22 33 44 78 99)
-- Nothing
example2 ::
  FiveOf Integer
  -> Maybe Integer
example2 is =
  let firstMod7 =
        findRightIncl (\x -> x `mod` 7 == 0) (toFiveOfZipper is)
      moveTo5 =
        getFocus . (fromInteger . getFocus >>= moveCycle)
  in  fmap moveTo5 firstMod7
