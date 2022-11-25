{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
  DerivingVia,
  FlexibleContexts,
  FlexibleInstances,
  PatternSynonyms,
  ScopedTypeVariables,
  StandaloneDeriving,
  UndecidableInstances #-}
module DiffLoc.Internal.Test
  ( (=.=)
  , partialSemiInverse
  , GoodSpan(.., GSN, GSV)
  , FairSpan(.., FSN, FSV)
  ) where

import Control.Applicative
import Data.Coerce
import Data.Foldable (toList)
import Data.List (sort)
import Test.QuickCheck

import DiffLoc.Internal.Diff
import DiffLoc.Internal.Colline
import DiffLoc.Internal.Shift

-- $setup
-- >>> import Data.Maybe
-- >>> import DiffLoc

infix 1 =.=

(=.=) :: (Arbitrary a, Show a, Eq b, Show b) => (a -> b) -> (a -> b) -> Property
f =.= g = property (\x -> f x === g x)

whenJust :: Testable prop => Maybe a -> (a -> prop) -> Property
whenJust Nothing _ = discard
whenJust (Just x) f = property (f x)

partialSemiInverse :: (Arbitrary a, Eq a, Show a) => (a -> Maybe b) -> (b -> Maybe a) -> a -> Property
partialSemiInverse f g x = f x `whenJust` \y -> g y === Just x

instance (Arbitrary v, Arbitrary (Point v)) => Arbitrary (Interval v) where
  arbitrary = do
    (i, n) <- arbitrary
    pure (i :.. n)
  shrink (i :.. n) = [ i' :.. n' | (i', n') <- shrink (i, n) ]

instance (Arbitrary v, Arbitrary (Point v)) => Arbitrary (Replace v) where
  arbitrary = do
    (i, n, m) <- arbitrary
    pure (Replace i n m)
  shrink (Replace i n m) =
    [ Replace i' n' m' | (i', n', m') <- shrink (i, n, m) ]

instance (Shift v, Arbitrary v) => Arbitrary (Diff v) where
  arbitrary = listToDiff <$> (arbitrary :: Gen [v])
  shrink (Diff d) = listToDiff <$> shrink (coerce (toList d))

listToDiff :: Shift r => [r] -> Diff r
listToDiff = foldr addReplace emptyDiff

-- | Generate GoodSpan most of the time, but also some completely arbitrary ones once in a while.
data FairSpan v = FS (Diff (Replace v)) (Interval v)

-- | A Diff and a non-conflicting Span
data GoodSpan v = GS (Diff (Replace v)) (Interval v)

deriving instance (Show v, Show (Point v)) => Show (FairSpan v)
deriving instance (Show v, Show (Point v)) => Show (GoodSpan v)

pattern FSN :: Diff (Replace (Plain Int)) -> Interval (Plain Int) -> FairSpan (Plain Int)
pattern FSN d s = FS d s

pattern GSN :: Diff (Replace (Plain Int)) -> Interval (Plain Int) -> GoodSpan (Plain Int)
pattern GSN d s = GS d s

pattern FSV :: Diff (Replace Vallee) -> Interval Vallee -> FairSpan Vallee
pattern FSV d s = FS d s

pattern GSV :: Diff (Replace Vallee) -> Interval Vallee -> GoodSpan Vallee
pattern GSV d s = GS d s

instance (Affine v, Arbitrary v, Arbitrary (Point v)) => Arbitrary (FairSpan v) where
  arbitrary = frequency [(10, (\(GS d s) -> FS d s) <$> arbitrary), (1, arbitrary)]

-- |
-- prop> \(GSN d s) -> isJust (mapDiff d s)
-- prop> \(GSV d s) -> isJust (mapDiff d s)
instance (Affine v, Arbitrary v, Arbitrary (Point v)) => Arbitrary (GoodSpan v) where
  arbitrary = do
    let pairs (x : y : xs) = (x, y) : pairs xs
        pairs _ = []
    ts <- pairs <$> sort <$> liftA2 (:) arbitrary (liftA2 (:) arbitrary arbitrary)
    i <- choose (0, length ts-1)
    let (s, ts') = case splitAt i ts of
          (pre, (x, y) : suf) -> (x :.. (y .-. x), pre ++ suf)
          _ -> error "should not happen"
    d <- listToDiff <$> traverse (\(x, y) -> Replace x (y .-. x) <$> arbitrary) ts'
    pure (GS d s)

deriving via NonNegative a instance (Arbitrary a, Num a, Ord a) => Arbitrary (Plain a)

instance Arbitrary Line where
  arbitrary = Line <$> getNonNegative <$> arbitrary

instance Arbitrary Col where
  arbitrary = Col <$> getNonNegative <$> arbitrary

instance Arbitrary Colline where
  arbitrary = Colline <$> arbitrary <*> arbitrary

instance Arbitrary Vallee where
  arbitrary = do
    (NonNegative l, NonNegative c) <- arbitrary
    pure (Vallee (Delta l) (Delta c))
