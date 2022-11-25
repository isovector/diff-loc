{-# LANGUAGE
  DerivingVia,
  DerivingStrategies,
  FlexibleContexts,
  GeneralizedNewtypeDeriving,
  StandaloneDeriving,
  TypeFamilies,
  UndecidableInstances #-}
module DiffLoc.Shift
  ( BlockOrder(..)
  , Shift(..)
  , Affine(..)
  , (.-.)
  , Interval(..)
  , isEmpty
  , Replace(..)
  , Delta(..)
  , deltaZero
  , delta
  , Plain(..)
  ) where

import Data.Coerce (Coercible, coerce)
import Data.Monoid (Sum(..))
import Prelude hiding ((+))
import qualified Prelude

-- $setup
-- >>> import Control.Monad ((<=<))
-- >>> import Test.QuickCheck
-- >>> import DiffLoc.Test
-- >>> type N = Plain Int

infixl 6 +
(+) :: Semigroup a => a -> a -> a
(+) = (<>)

-- | Ordering of interval-like things.
class BlockOrder b where
  precedes :: b -> b -> Bool

  -- | Precedes but not adjacent, provided you have a notion of adjacence.
  -- Otherwise it's fine to equate this with precedes.
  distantlyPrecedes :: b -> b -> Bool

-- | Shift algebra.
--
-- __Laws:__
--
-- @
-- 'src' \<$>   'shiftR' r s   =     'shiftBlock' r ('src' s)
-- 'tgt' \<$>   'shiftR' r s   =     'shiftBlock' r ('tgt' s)
-- 'src' \<$> 'coshiftR' r s   =   'coshiftBlock' r ('src' s)
-- 'tgt' \<$> 'coshiftR' r s   =   'coshiftBlock' r ('tgt' s)
--
-- 'shiftBlock' r b = Just d   \<==>   'coshiftBlock' r d = Just b
-- 'shiftR'     r s = Just z   \<==>   'coshiftR'     r z = Just s
--
-- 'shiftR' r s = Just z  &&  'shiftR' s r = Just q   ==>
--   z '<>' r  =  q '<>' s
--
-- 'coshiftR' r s = Just z  &&  'coshiftR' s r = Just q   ==>
--   r '<>' z  =  s '<>' q
-- @
--
-- __Duality laws:__
--
-- @
-- src = tgt . 'dual'
-- tgt = src . 'dual'
-- shiftBlock = coshiftBlock . 'dual'
-- coshiftBlock = shiftBlock . 'dual'
-- coshiftR = shiftR . 'dual'
-- shiftR = coshiftR . 'dual'
-- @
class (Semigroup r, BlockOrder (Block r)) => Shift r where
  type Block r
  src :: r -> Block r
  tgt :: r -> Block r

  shiftBlock   :: r -> Block r -> Maybe (Block r)
  coshiftBlock :: r -> Block r -> Maybe (Block r)

  shiftR :: r -> r -> Maybe r
  coshiftR :: r -> r -> Maybe r

  dual :: r -> r

  src = tgt . dual
  tgt = src . dual
  shiftBlock = coshiftBlock . dual
  coshiftBlock = shiftBlock . dual
  coshiftR = shiftR . dual
  shiftR = coshiftR . dual

-- | Ordered affine spaces are made of points and vectors.
--
-- - Vectors can be added, that's the 'Semigroup' superclass.
-- - Points can be translated along vectors using @('.+')@.
-- - Given two ordered points @i <= j@, @j '.-.?' i@ finds a vector @n@
--   such that @i + n = j@.
--
-- In other words, we only require the existence of "positive" vectors.
-- This makes it possible to implement this class for line-column locations
-- ("DiffLoc.Colline").
--
-- __Laws:__
--
-- @
--               (x '.+' v) '.+' w  =  x '.+' (v '<>' w)
-- x '<=' y  ==>  x '.+' (y '.-.' x)  =  y
--              (x '.+' v) '.-.' x  =  x
-- @
class (Ord (Point v), Semigroup v) => Affine v where
  type Point v

  infixr 6 .+

  -- | Translation.
  (.+) :: Point v -> v -> Point v

  -- | Vector between two points.
  -- @j .-.? i@ must be defined ('Just') if @i <= j@,
  (.-.?) :: Point v -> Point v -> Maybe v

-- $hidden
-- prop> (x .+ v) .+ w  ===  x .+ (v <> w :: Plain Int)
-- prop> x <= y  ==>  x .+ (y .-. x :: Plain Int)  ===  y
-- prop> (x .+ v) .-. x   ===   (v :: Plain Int)

infixl 6 .-.

-- | A variant of @('.-.?')@ which throws an exception on @Nothing@.
(.-.) :: Affine v => Point v -> Point v -> v
i .-. j = case i .-.? j of
  Nothing -> error "undefined vector"
  Just n -> n

infixl 3 :..

-- | @i ':..' n@ is the interval @[i, i+n]@.
--
-- Note that the length @n@ may be zero.
--
-- The elements of the interval can be thought of as indexing the interstices
-- /between/ characters. A span of length zero is a single interstice between
-- two characters, where some chunk of text may be inserted.
--
-- Example: drawing of @1 :.. 2@ in "abcde".
--
-- >  a b c d e
-- > 0 1 2 3 4 5
-- >   ^b+c+ length = 2
-- >   ^
-- >   ^ start = 1
data Interval v = !(Point v) :.. !v

isEmpty :: (Eq v, Monoid v) => Interval v -> Bool
isEmpty (_ :.. n) = n == mempty

deriving instance (Eq (Point v), Eq v) => Eq (Interval v)
deriving instance (Show (Point v), Show v) => Show (Interval v)

instance Affine v => BlockOrder (Interval v) where
  precedes (i :.. n) (j :.. _) = i .+ n <= j
  distantlyPrecedes (i :.. n) (j :.. _) = i .+ n < j

-- | A minimalistic representation of text replacements.
--
-- A replacement @'Replace' i n m@ is given by a start location @i@, the length
-- @n@ of the interval to replace (source) and the length @m@ of its
-- replacement (target).
-- This representation does not keep track of the actual data being inserted.
--
-- This may overapproximate the underlying text replacement,
-- with intervals being wider than necessary.
-- For example, the transformation from "abc" to "ac" could be represented
-- by @mkReplace 1 1 0@ (replace "b" with "" at location 1), and also by
-- @mkReplace 0 2 1@ (replace "ab" with "a" at location 0).
--
-- > source   abc   abc
-- >      -    b    ab
-- >      +         a
-- > target   a c   a c
--
-- Insertions are replacements with source intervals of length zero.
-- Deletions are replacements with target intervals of length zero.
--
-- === __Composition__
--
-- Replacements can be composed using 'Semigroup'.
data Replace v = Replace !(Point v) !v !v

deriving instance (Eq (Point v), Eq v) => Eq (Replace v)
deriving instance (Show (Point v), Show v) => Show (Replace v)

-- | The composition of two replacements @l <> r@ represents the replacement @r@
-- followed by @l@, as one replacement of an span that contains both @r@ and @l@.
--
-- The right-to-left order of composition has the nice property that when
-- @l `'precedes'` r@, @l <> r@ can also be viewed intuitively as performing @l@ and
-- @r@ simultaneously.
--
-- === Properties
--
-- prop> (x <> y) <> z === x <> (y <> z :: Replace (Plain Int))
instance Affine v => Semigroup (Replace v) where
  Replace li ln lm <> Replace ri rn rm
    | li .+ ln <= ri
      -- Disjoint, l on the left.
      --
      -- Before:
      -- > |---l---|       |---r---|
      -- > li      li+ln   ri      ri+rn
      --
      -- After both replacements (r first),
      -- with ld = lm-ln
      --
      -- > |---l---|       |---r---|
      -- > li      li+lm   ri+ld   ri+rm+ld
      --
    = Replace li ((ri .+ rn) .-. li) (lm + (ri .-. (li .+ ln)) + rm)

    | li <= ri
      -- l straddles the left end of r
      --
      -- Note that the indices in l should be interpreted
      -- as indices after r.
      -- After replacing r, the replaced span r and the to-be-replaced
      -- span l look like this:
      --
      -- >       |------r----|
      -- > |----l-----|
      -- > li    ri   li+ln  ri+rm
      --
      -- or this:
      --
      -- >      |--r--|
      -- > |-------l----------|
      -- > li   ri    ri+rm   li+ln
      --
    = let (n, m) = if li .+ ln < ri .+ rm
                   then ((ri .+ rn) .-. li, lm + ((ri .+ rm) .-. (li .+ ln)))
                   else ((ri .-. li) + rn + ((li .+ ln) .-. (ri .+ rm)), lm)
      in Replace li n m

    | li < ri .+ rm
      -- r straddles the left end of l
      --
      -- > |----r-----|
      -- >       |------l----|
      -- > ri    li   ri+rm  li+ln
      --
      -- or
      --
      -- > |-------r----------|
      -- >      |--l--|
      -- > ri   li    li+ln   ri+rm
      --
    = let (n, m) = if ri .+ rm < li .+ ln
                   then (rn + ((li .+ ln) .-. (ri .+ rm)), (li .+ lm) .-. ri)
                   else (rn, (li .-. ri) + lm + ((ri .+ rm) .-. (li .+ ln)))
      in Replace ri n m

    | otherwise
      --
      -- > |---r---|       |---l---|
      -- > ri      rm      li      ln
      --
    = Replace ri (rn + (li .-. (ri .+ rm)) + ln) ((li .+ lm) .-. ri)

instance Affine v => Shift (Replace v) where
  type Block (Replace v) = Interval v
  dual (Replace i n m) = Replace i m n

  src (Replace i n _) = i :.. n

  shiftBlock (Replace i n m) jp@(j :.. p)
    | j .+ p <= i = Just jp
    | i .+ n <= j = Just (i .+ (m + (j .-. (i .+ n))) :.. p)
    | otherwise = Nothing

  shiftR (Replace i n m) jpq@(Replace j p q)
    | j .+ p <= i = Just jpq
    | i .+ n <= j = Just (Replace (i .+ (m + (j .-. (i .+ n)))) p q)
    | otherwise = Nothing

-- | A phantom type for representing differences between @Int@ quantities.
newtype Delta a = Delta Int
  deriving (Eq, Ord, Show)
  deriving (Semigroup, Monoid) via (Sum Int)

deltaZero :: Delta a
deltaZero = Delta 0

-- | Difference between two quantities.
delta :: Coercible a Int => a -> a -> Delta a
delta y x = Delta (coerce y - coerce x)

--

-- | One-dimensional indices.
newtype Plain a = Plain a
  deriving (Eq, Ord)
  deriving newtype (Num, Show)
  deriving (Semigroup, Monoid) via (Sum a)

instance (Num a, Ord a) => Affine (Plain a) where
  type Point (Plain a) = a
  y .+ Plain x = x Prelude.+ y
  x .-.? y | y <= x = Just (Plain (x - y))
           | otherwise = Nothing
