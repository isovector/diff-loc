{-# OPTIONS_GHC -Wno-orphans #-}
module DiffLoc.Test
  ( (=.=)
  , partialSemiInverse
  ) where

import Data.Foldable (toList)
import Test.QuickCheck

import DiffLoc
import DiffLoc.Internal

infix 1 =.=

(=.=) :: (Arbitrary a, Show a, Eq b, Show b) => (a -> b) -> (a -> b) -> Property
f =.= g = property (\x -> f x === g x)

whenJust :: Testable prop => Maybe a -> (a -> prop) -> Property
whenJust Nothing _ = discard
whenJust (Just x) f = property (f x)

partialSemiInverse :: (Arbitrary a, Eq a, Show a) => (a -> Maybe b) -> (b -> Maybe a) -> Property
partialSemiInverse f g = property (\x -> f x `whenJust` \y -> g y === Just x)

instance Arbitrary Span where
  arbitrary = mkSpan <$> arbitrary <*> (getNonNegative <$> arbitrary)

instance Arbitrary Replace where
  arbitrary = mkReplace
    <$> arbitrary
    <*> (getNonNegative <$> arbitrary)
    <*> (getNonNegative <$> arbitrary)

instance Arbitrary Diff where
  arbitrary = listToDiff <$> (arbitrary :: Gen [Replace])
  shrink (Diff d) = listToDiff <$> shrink (toList d)

listToDiff :: [Replace] -> Diff
listToDiff = foldr addReplace emptyDiff
