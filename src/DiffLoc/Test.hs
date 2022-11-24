{-# OPTIONS_GHC -Wno-orphans #-}
module DiffLoc.Test
  ( (=.=)
  , partialSemiInverse
  , GoodSpan(..)
  , FairSpan(..)
  ) where

import Control.Applicative
import Data.Foldable (toList)
import Data.List (sort)
import Test.QuickCheck

import DiffLoc
import DiffLoc.Internal

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

instance Arbitrary Span where
  arbitrary = do
    (i, NonNegative n) <- arbitrary
    pure (mkSpan i n)
  shrink (Span i n) = [ Span i' n' | (i', NonNegative n') <- shrink (i, NonNegative n) ]

instance Arbitrary Replace where
  arbitrary = do
    (i, NonNegative n, NonNegative m) <- arbitrary
    pure (mkReplace i n m)
  shrink (Replace i n m) =
    [ Replace i' n' m' | (i', NonNegative n', NonNegative m') <- shrink (i, NonNegative n, NonNegative m) ]

instance Arbitrary Diff where
  arbitrary = listToDiff <$> (arbitrary :: Gen [Replace])
  shrink (Diff d) = listToDiff <$> shrink (toList d)

listToDiff :: [Replace] -> Diff
listToDiff = foldr addReplace emptyDiff

-- | Generate GoodSpan most of the time, but also some completely arbitrary ones once in a while.
data FairSpan = FS Diff Span
  deriving Show

instance Arbitrary FairSpan where
  arbitrary = frequency [(10, (\(GS d s) -> FS d s) <$> arbitrary), (1, arbitrary)]

-- | A Diff and a non-conflicting Span
data GoodSpan = GS Diff Span
  deriving Show

-- |
-- prop> \(GS d s) -> isJust (mapDiff d s)
instance Arbitrary GoodSpan where
  arbitrary = do
    let pairs (x : y : xs) = (x, y) : pairs xs
        pairs _ = []
        walk 0 ((x, y) : xs) | x == y = (mkSpan (x+1) 0, (\(a, b) -> (a+1, b+1)) <$> xs)
                             | otherwise = (mkSpan x (y-x), xs)
        walk i (xy : xs) = (s, xy : ys)
          where (s, ys) = walk (i-1) xs
        walk _ [] = error "should not happen"
    ts <- pairs <$> sort <$> liftA2 (:) arbitrary (liftA2 (:) arbitrary arbitrary)
    i <- choose (0, length ts-1)
    let (s, ts') = walk i ts
    d <- listToDiff <$> traverse (\(x, y) -> mkReplace x (y-x) <$> getNonNegative <$> arbitrary) ts'
    pure (GS d s)
