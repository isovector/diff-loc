{-# LANGUAGE
  AllowAmbiguousTypes,
  DataKinds,
  DerivingStrategies,
  DerivingVia,
  GeneralizedNewtypeDeriving,
  ScopedTypeVariables,
  TypeApplications,
  TypeFamilies #-}
module DiffLoc.Index
  ( -- * One-dimensional indices
    -- ** Unbounded indices
    Plain(..)

    -- ** Indices bounded by an origin
  , IndexFrom(..)
  , indexFrom
  , indexFrom0
  , indexFrom1
  , fromIndex
  , fromIndex0
  , fromIndex1
  , zeroIndex
  , oneIndex

    -- ** Offsets
  , Offset()
  , offset
  ) where

import Data.Coerce (Coercible, coerce)
import Data.Monoid (Sum(..))
import Data.Proxy (Proxy(..))
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat, Nat, natVal)
import DiffLoc.Shift

-- | One-dimensional indices.
newtype Plain a = Plain a
  deriving (Eq, Ord, Show)

instance (Num a, Ord a) => Affine (Plain a) where
  type Trans (Plain a) = Offset a
  Plain y .+ Offset x = Plain (x Prelude.+ y)
  Plain x .-.? Plain y | y <= x = Just (Offset (x - y))
                       | otherwise = Nothing

--

-- | One-dimensional index with an origin.
-- Indices must be greater than the origin, hence the constructor is hidden.
--
-- Use the 'indexFrom' function instead to construct indices, with
-- @TypeApplications@ to make the indexing scheme explicit.
--
-- @
-- origin @(IndexFrom n a) <= i
-- @
newtype IndexFrom (n :: Nat) a = IndexFrom a
  deriving (Eq, Ord, Show)
  deriving Affine via (Plain a)

instance (Num a, Ord a, KnownNat n) => Origin (IndexFrom n a) where
  origin = IndexFrom (knownNum @n)

knownNum :: forall n a. (KnownNat n, Num a) => a
knownNum = fromIntegral (natVal @n Proxy)

-- | Constructor for 'IndexFrom'. The index must be greater than the origin,
-- otherwise an error is raised.
--
-- @
-- origin <= indexFrom i
-- @
indexFrom :: forall n a. (HasCallStack, KnownNat n, Num a, Ord a) => a -> IndexFrom n a
indexFrom i | knownNum @n <= i = IndexFrom i
            | otherwise = error ("IndexFrom must not be less than origin " <> show (natVal @n Proxy))

indexFrom0 :: (HasCallStack, Num a, Ord a) => a -> IndexFrom 0 a
indexFrom0 = indexFrom

indexFrom1 :: (HasCallStack, Num a, Ord a) => a -> IndexFrom 1 a
indexFrom1 = indexFrom

-- | Destructor for 'IndexFrom'.
fromIndex :: forall n a. IndexFrom n a -> a
fromIndex (IndexFrom i) = i

fromIndex0 :: IndexFrom 0 a -> a
fromIndex0 = fromIndex

fromIndex1 :: IndexFrom 1 a -> a
fromIndex1 = fromIndex

-- | Convert from zero-indexing to one-indexing.
oneIndex :: Num a => IndexFrom 0 a -> IndexFrom 1 a
oneIndex (IndexFrom i) = IndexFrom (i Prelude.+ 1)

-- | Convert from one-indexing to zero-indexing.
zeroIndex :: Num a => IndexFrom 1 a -> IndexFrom 0 a
zeroIndex (IndexFrom i) = IndexFrom (i - 1)

-- | Type of nonnegative offsets.
newtype Offset a = Offset a
  deriving (Eq, Ord, Show)
  deriving (Semigroup, Monoid) via (Sum a)

-- | Construct a nonnegative 'Offset'. Otherwise an error is raised.
offset :: (HasCallStack, Num a, Ord a) => a -> Offset a
offset i | 0 <= i = Offset i
         | otherwise = error "Offset must not be negative"
