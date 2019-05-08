{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Z02 where

import Z00(FiveOfZipper(FiveOfZipper), FiveOfDerivative(FiveOfDerivative), UpToFive(One, Two, Three, Four, Five))
import qualified Z00 as Z00
import Z01(ListZipper(ListZipper), ListDerivative(ListDerivative))
import qualified Z01 as Z01

-- | Cojoin is any functor that also supports the `cojoin` operation.
--
-- /Tip/ `cojoin` is also sometimes called `duplicate`
class Functor f => Cojoin f where
  cojoin ::
    f x
    -> f (f x)

-- | Comonad is any cojoin that also supports the `copure` operation.
--
-- /Tip/ `copure` is also sometimes called `extract`
class Cojoin f => Comonad f where
  copure ::
    f x
    -> x

---- standard library examples

-- There is no `Comonad` for `[]` but there is `Cojoin`.
-- i.e. `instance Comonad []` cannot be written, but this can.
--
-- >>> cojoin [1,2,3]
-- [[1,2,3],[2,3],[3]]
instance Cojoin [] where
  cojoin [] =
    []
  cojoin x@(_:t) =
    x : cojoin t

-- There is no `Comonad` for `Maybe` but there is `Cojoin`.
-- i.e. `instance Comonad Maybe` cannot be written, but this can.
--
-- >>> cojoin Nothing
-- Nothing
--
-- >>> cojoin (Just "abc")
-- Just (Just "abc")
instance Cojoin Maybe where
  cojoin Nothing =
    Nothing
  cojoin x@(Just _) =
    Just x

-- The `((,) a)` functor can cojoin. i.e. `(a, x) -> (a, (a, x))`
--
-- >>> cojoin (99, "abc")
-- (99,(99,"abc"))
instance Cojoin ((,) a) where
  cojoin (a, x) =
    (a, (a, x))

-- The `((,) a)` cojoin can copure. i.e. `(a, x) -> x`
--
-- >>> copure (99, "abc")
-- "abc"
instance Comonad ((,) a) where
  copure (_, x) =
    x

----

-- | The `extend` function relates to `cojoin` in that one can be written in terms of the other.
-- In this case, we choose to write `extend` in terms of `cojoin`.
--
-- /Tip/ Remember that you also have `fmap` available.
--
-- >>> extend id [1,2,3]
-- [[1,2,3],[2,3],[3]]
--
-- >>> extend id Nothing
-- Nothing
--
-- >>> extend id (Just "abc")
-- Just (Just "abc")
--
-- >>> extend id (99, "abc")
-- (99,(99,"abc"))
--
-- >>> extend sum [105,202,307]
-- [614,509,307]
extend ::
  Cojoin f =>
  (f x -> y)
  -> f x
  -> f y
extend =
  error "todo: Z02#extend"

-- | Implement composition. Note that this type signature is the same as the signature for
-- (.) :: (f y -> z) -> (f x -> y) -> f x -> z
-- However, the functor wraps every argument position.
--
-- >>> comonadCompose sum sum [99,88,77]
-- 506
--
-- >>> comonadCompose (uncurry (+)) (uncurry (*)) (88, 99)
-- 8800
comonadCompose ::
  Cojoin f =>
  (f y -> z)
  -> (f x -> y)
  -> f x
  -> z
comonadCompose =
  error "todo: Z02#comonadCompose"

---- All zippers are comonads!

---- Comonad laws

-- | A test of `cojoin` associativity. All instances of `Cojoin` must have an
-- implementation that ensures that this function returns `Nothing`.
--
-- If the test fails, two unequal values (which should be equal) are returned in `Just`.
law1 ::
  (Cojoin f, Eq (f (f (f x)))) =>
  f x
  -> Maybe (f (f (f x)), f (f (f x)))
law1 x =
  let r = cojoin (cojoin x) 
      s = fmap cojoin (cojoin x)
  in  if r == s
        then
          Nothing
        else
          Just (r, s)

-- | A test of `cojoin` and `copure` that should always return `Nothing`.
-- If the test fails, two unequal values (which should be equal) are returned in `Just`.
law2 ::
  (Comonad f, Eq (f x)) =>
  f x
  -> Maybe (f x, f x)
law2 x =
  let x' = copure (cojoin x)
  in  if x == x'
        then
          Nothing
        else
          Just (x, x')

-- | A test of `cojoin` and `copure` that should always return `Nothing`.
-- If the test fails, two unequal values (which should be equal) are returned in `Just`.
law3 ::
  (Comonad f, Eq (f x)) =>
  f x
  -> Maybe (f x, f x)
law3 x =
  let x' = fmap copure (cojoin x)
  in  if x == x'
        then
          Nothing
        else
          Just (x, x')

-- | Implement `cojoin` for `FiveOfZipper`. Ensure that `law1` passes.
--
-- /Tip/ We already wrote this function in `module Z00`
instance Cojoin FiveOfZipper where
  cojoin =
    error "todo: Z02#FiveOfZipper.cojoin"

-- | Implement `cojoin` for `ListZipper`. Ensure that `law1` passes.
--
-- /Tip/ We already wrote this function in `module Z01`
--
-- >>> cojoin (FiveOfZipper 10 (FiveOfDerivative Five 11 12 13 14))
-- FiveOfZipper (FiveOfZipper 10 (FiveOfDerivative One 11 12 13 14)) (FiveOfDerivative Five (FiveOfZipper 10 (FiveOfDerivative Two 11 12 13 14)) (FiveOfZipper 10 (FiveOfDerivative Three 11 12 13 14)) (FiveOfZipper 10 (FiveOfDerivative Four 11 12 13 14)) (FiveOfZipper 10 (FiveOfDerivative Five 11 12 13 14)))
--
-- >>> cojoin (ListZipper 1 (ListDerivative [] [2,3,4,5]))
-- ListZipper (ListZipper 1 (ListDerivative [] [2,3,4,5])) (ListDerivative [] [ListZipper 2 (ListDerivative [1] [3,4,5]),ListZipper 3 (ListDerivative [2,1] [4,5]),ListZipper 4 (ListDerivative [3,2,1] [5]),ListZipper 5 (ListDerivative [4,3,2,1] [])])
--
-- >>> cojoin (ListZipper 3 (ListDerivative [2,1] [4,5]))
-- ListZipper (ListZipper 3 (ListDerivative [2,1] [4,5])) (ListDerivative [ListZipper 2 (ListDerivative [1] [3,4,5]),ListZipper 1 (ListDerivative [] [2,3,4,5])] [ListZipper 4 (ListDerivative [3,2,1] [5]),ListZipper 5 (ListDerivative [4,3,2,1] [])])
--
-- >>> cojoin (ListZipper 5 (ListDerivative [4,3,2,1] []))
-- ListZipper (ListZipper 5 (ListDerivative [4,3,2,1] [])) (ListDerivative [ListZipper 4 (ListDerivative [3,2,1] [5]),ListZipper 3 (ListDerivative [2,1] [4,5]),ListZipper 2 (ListDerivative [1] [3,4,5]),ListZipper 1 (ListDerivative [] [2,3,4,5])] [])
instance Cojoin ListZipper where
  cojoin =
    error "todo: Z02#ListZipper.cojoin"

-- | Implement `copure` for `FiveOfZipper`. Ensure that `law1`, `law2` and `law3` passes.
--
-- /Tip/ We already wrote this function in `module Z00`
--
-- >>> copure (FiveOfZipper 10 (FiveOfDerivative One 11 12 13 14))
-- 10
--
-- >>> copure (FiveOfZipper 10 (FiveOfDerivative Three 11 12 13 14))
-- 12
--
-- >>> copure (FiveOfZipper 10 (FiveOfDerivative Five 11 12 13 14))
-- 14
instance Comonad FiveOfZipper where
  copure =
    error "todo: Z02#FiveOfZipper.copure"

-- | Implement `copure` for `ListZipper`. Ensure that `law1`, `law2` and `law3` passes.
--
-- /Tip/ We already wrote this function in `module Z01`
--
-- copure (ListZipper 5 (ListDerivative [4,3,2,1] []))
-- 5
--
-- >>> copure (ListZipper 3 (ListDerivative [2,1] [4,5]))
-- 3
--
-- >>> copure (ListZipper 1 (ListDerivative [] [2,3,4,5]))
-- 1
instance Comonad ListZipper where
  copure =
    error "todo: Z02#ListZipper.copure"
