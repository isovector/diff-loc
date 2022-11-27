{-# LANGUAGE
  DataKinds,
  DerivingVia,
  FlexibleInstances,
  StandaloneDeriving,
  TypeOperators #-}
module DiffLoc.Starter
  ( Diff
  , (:$:)(..)

    -- * Common shorthands
  , Z
  , N
  , N'
  ) where

import Data.Coerce (Coercible)
import DiffLoc.Diff
import DiffLoc.Interval
import DiffLoc.Index
import DiffLoc.Shift

-- | A shorthand for the common use case of 'Diff'.
type Diff p = ADiff (Replace p)

-- | A trick to reduce noise by hiding newtype wrapper constructors.
--
-- >>> show (NoShow (Plain 3) :: Plain :$: Int)
-- "3"
-- >>> show (Colline 4 2 :.. Vallee (offset 3) (offset 3) :: Colline N N)
-- "Colline 4 2 :.. Vallee 3 3
newtype f :$: x = NoShow (f x)
  deriving (Eq, Ord)
  deriving (Semigroup, Monoid, Affine, Origin) via (f x)

deriving via a instance (Show a, Coercible (f a) a) => Show (f :$: a)
deriving via a instance (Num a, Coercible (f a) a) => Num (f :$: a)  -- ^ For the literals


-- | Integers.
type Z = Plain :$: Int

-- | Natural numbers.
type N = IndexFrom 0 :$: Int

-- | Positive numbers.
type N' = IndexFrom 1 :$: Int
