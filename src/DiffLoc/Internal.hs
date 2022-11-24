{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module DiffLoc.Internal where

import Data.FingerTree (FingerTree)
import qualified Data.FingerTree as FT

-- $setup
-- >>> import Control.Monad ((<=<))
-- >>> import Test.QuickCheck
-- >>> import DiffLoc.Test

-- | @'mkSpan' s n@ represents a span of text with start location @s@ with length @n@.
-- Note that spans of length zero are still considered distinct if their
-- locations are distinct.
--
-- A span can be thought of as the interval @[s, s+n]@, where the elements are
-- indices into the interstices /between/ characters. A span of length zero
-- is a single interstice between two characters, where some chunk of text may
-- be inserted.
--
-- Example: drawing of @mkSpan 1 2@ in "abcde".
--
-- >  a b c d e
-- >   ^-+-+ length = 2
-- >   ^
-- >   ^ start = 1
data Span = Span Int Int
  deriving (Eq, Ord, Show)

-- | Smart constructor for 'Span'.
--
-- __Condition__: the span length must be nonnegative.
mkSpan :: Int {- ^ Start location -} -> Int {- ^ Length -} -> Span
mkSpan _ l | l < 0 = error "diff-loc: negative span length"
mkSpan s l = Span s l

-- | Start location of a 'Span'.
spanStart :: Span -> Int
spanStart (Span s _) = s

-- | Length of a 'Span'.
spanLength :: Span -> Int
spanLength (Span _ n) = n

-- | @l `precedes` r@ if @l@ and @r@ are disjoint and is to the left of @r@.
precedes :: Span -> Span -> Bool
precedes (Span i n) (Span j _) = i + n <= j

-- | @l `distantlyPrecedes` r@ if @l `precedes` r@ and they don't touch.
distantlyPrecedes :: Span -> Span -> Bool
distantlyPrecedes (Span i n) (Span j _) = i + n < j

-- | Don't allow @s2@ to be at the right end of @s1@ if @s2@ is empty.
-- Using this to check for conflicts in @mapDiff@ and @mapReplace@ allows us
-- to merge adjacent replacements in @addReplace@.
precedes' :: Span -> Span -> Bool
precedes' s1 s2 | spanLength s2 == 0 = s1 `distantlyPrecedes` s2
                | otherwise = s1 `precedes` s2

-- | A minimalistic representation of text replacements.
--
-- A replacement @'mkReplace' i n m@ is given by a start location @i@, the length
-- @n@ of the span to replace (source) and the length @m@ of its
-- replacement (target).
-- This representation does not keep track of the actual data being inserted.
--
-- This may coarsely overapproximate the underlying text replacement,
-- with spans being wider than necessary.
-- For example, the transformation from "abc" to "ac" could be represented
-- by @mkReplace 1 1 0@ (replace "b" with "" at index 1), and also by
-- @mkReplace 0 2 1@ (replace "ab" with "a" at index 0).
--
-- Insertions are replacements with source spans of length zero.
-- Deletions are replacements with target spans of length zero.
--
-- === __Composition__
--
-- Replacements can be composed using 'Semigroup'. @l <> r@ is the result
-- of performing @r@ then @l@. Since this representation is span-based,
-- it can be a quite coarse overapproximation.
-- The composed replacement @l <> r@ consists of a source span that covers
-- the source spans of both operands. Note that order matters, because
-- replacements change the indices of elements.
--
-- The right-to-left order of composition has the nice property that when @l@
-- and @r@ are disjoint and @l@ is located to the left of @r@, @l <> r@ can
-- also be viewed intuitively as performing @l@ and @r@ simultaneously.
data Replace = Replace { start :: Int, srcLength :: Int, tgtLength :: Int }
  deriving (Eq, Show)

-- | Smart constructor for 'Replace'.
--
-- __Condition__: the source and target lengths must be nonnegative.
mkReplace :: Int {- ^ Start location -} -> Int {- ^ Source length -} -> Int {- ^ Target length -} -> Replace
mkReplace _ n _ | n < 0 = error "diff-loc: negative source length"
mkReplace _ _ m | m < 0 = error "diff-loc: negative target length"
mkReplace s n m = Replace s n m

-- | Starting point of a 'Replace'.
--
-- __Condition__: not 'isEmpty'.
replaceStart :: Replace -> Int
replaceStart = start

-- | Source length of a 'Replace'.
replaceSrcLength :: Replace -> Int
replaceSrcLength = srcLength

-- | Target length of a 'Replace'.
replaceTgtLength :: Replace -> Int
replaceTgtLength = tgtLength

-- | The source span.
unsafeSrc :: Replace -> Span
unsafeSrc (Replace i n _) = Span i n

-- | The target span: the location of the result of the replacement.
unsafeTgt :: Replace -> Span
unsafeTgt (Replace i _ m) = Span i m

-- | Whether a replacement is empty.
isEmpty :: Replace -> Bool
isEmpty (Replace _ n m) = n == 0 && m == 0

-- | A diff represents a transformation from one file to another.
--
-- Example diff between "abcdefgh" and "appcfgqqh":
--
-- > a b  c d e f g    h
-- >  |11\ |222|    /\
-- >  |11|  \2/    |33|
-- > a pp c     f g qq h
--
-- It consists of three replacements:
--
-- - replace "b" with "pp" at index 1, @mkReplace 1 1 2@;
-- - replace "de" with "" at index 3, @mkReplace 3 2 0@;
-- - replace "" with "qq" at index 7, @mkReplace 7 0 2@.
--
-- A diff is an sequence of /disjoint/ and /nonempty/ replacements,
-- /ordered/ by their source locations.
newtype Diff = Diff Diff_
  deriving (Eq, Show)

-- | The empty diff.
emptyDiff :: Diff
emptyDiff = Diff FT.empty

-- | The monoid annotation in the fingertree gives the endpoints of the replacements.
type Diff_ = FingerTree (Maybe Replace) Replace

-- | The composition of two replacements @l <> r@ represents the replacement @r@
-- followed by @l@, as one replacement of an span that contains both @r@ and @l@.
--
-- We choose the right-to-left order because it coincides with the parallel
-- composition of disjoint replacements, when @l@ is actually located on the
-- left of @r@.
--
-- In fact, we only implement the cases where the start location of @l@ is on
-- the start location of @r@, carefully avoiding other cases in the
-- implementation of @Diff@.
--
-- prop> \x y z -> (x <> y) <> z === x <> (y <> z :: Replace)
instance Semigroup Replace where
  Replace li ln lm <> Replace ri rn rm
    | li + ln <= ri
      -- Disjoint, l on the left.
      --
      -- Before:
      -- |---l---|       |---r---|
      -- li      li+ln   ri      ri+rn
      --
      -- After both replacements (r first),
      -- with ld = lm-ln
      --
      -- |---l---|       |---r---|
      -- li      li+lm   ri+ld   ri+rm+ld
    = Replace li (ri+rn-li) (ri+rm+(lm-ln)-li)

    | li <= ri
      -- l straddles the left end of r
      --
      -- Note that the indices in l should be interpreted
      -- as indices after r.
      -- After replacing r, the replaced span r and the to-be-replaced
      -- span l look like this:
      --
      --       |------r----|
      -- |----l-----|
      -- li    ri   li+ln  ri+rm
      --
      -- or this:
      --
      --      |--r--|
      -- |-------l----------|
      -- li   ri    ri+rm   li+ln
      --
    = let (n, m) = if li+ln < ri+rm then (ri+rn-li, ri+rm+lm-ln-li) else (ln-rm+rn, lm)
      in Replace li n m

    | li < ri+rm
      -- r straddles the left end of l
      --
      -- |----r-----|
      --       |------l----|
      -- ri    li   ri+rm  li+ln
      --
      -- or
      --
      -- |-------r----------|
      --      |--l--|
      -- ri   li    li+ln   ri+rm
    = let (n, m) = if ri+rm < li+ln then (li+ln-rm+rn-ri, li+lm-ri) else (rn, rm+lm-ln)
      in Replace ri n m

    | otherwise
      -- |---r---|       |---l---|
      -- ri      rm      li      ln
    = Replace ri (li+ln-rm+rn-ri) (li+lm-ri)

instance FT.Measured (Maybe Replace) Replace where
  measure = Just

addReplaceL :: Replace -> Diff_ -> Diff_
addReplaceL r d0 = case FT.viewl d0 of
  FT.EmptyL -> FT.singleton r
  s FT.:< d | unsafeSrc r `distantlyPrecedes` unsafeSrc s -> r FT.<| d0 -- merge adjacent replacements
            | otherwise -> addReplaceL (r <> s) d

-- | Add a replacement to a diff. The replacement is performed /after/ the diff.
--
-- Properties:
--
-- prop> \r d -> mapDiff (addReplace r d) =.= (mapReplace r <=< mapDiff d)
-- prop> \r d -> comapDiff (addReplace r d) =.= (comapDiff d <=< comapReplace r)
--
-- where @(=.=)@ is pointwise equality of functions.
addReplace :: Replace -> Diff -> Diff
addReplace r d | isEmpty r = d
addReplace r (Diff d) = Diff $ case FT.search (\r1 _-> r1 `notPrecedes_` r) d of
  FT.Position d1 s d2 -> d1 <> addReplaceL (shiftReplace (- (deltaDiff (Diff d1))) r) (s FT.<| d2)
  FT.OnLeft -> addReplaceL r d
  FT.OnRight -> d FT.|> shiftReplace (- (deltaDiff (Diff d))) r
  FT.Nowhere -> error "Broken invariant"
  where
    notPrecedes_ Nothing _ = False
    notPrecedes_ (Just r1) i = not (unsafeTgt r1 `precedes` unsafeTgt i)

-- | Length difference of the replacement (target length - source length).
deltaReplace :: Replace -> Int
deltaReplace (Replace _ n m) = m - n

-- | Length difference of a diff (target length - source length).
deltaDiff :: Diff -> Int
deltaDiff (Diff d) = case FT.measure d of
  Nothing -> 0
  Just r -> deltaReplace r

-- | Move the start location of an span.
shift :: Int -> Span -> Span
shift k (Span i n) = Span (i + k) n

-- | Move the start location of a replacement.
shiftReplace :: Int -> Replace -> Replace
shiftReplace k (Replace i n m) = Replace (i + k) n m

-- | Translate a span in the source of a diff to a span in its target.
-- @Nothing@ if the span overlaps with a replacement.
--
-- For exaple, given the following 'Diff' (or 'Replace') from "aAacCc" to "aAabbbcCc":
--
-- > a A a     c C c
-- >       / \
-- >      |   |
-- > a A a bbb c C c
--
-- >>> r0 = mkReplace 3 0 3
-- >>> d0 = addReplace r0 emptyDiff
--
-- The span of \"A\" remains unchanged
--
-- >>> mapDiff d0 (mkSpan 1 1)
-- Just (Span 1 1)
-- >>> mapReplace r0 (mkSpan 1 1)
-- Just (Span 1 1)
-- >>> comapDiff d0 (mkSpan 1 1)
-- Just (Span 1 1)
-- >>> comapReplace r0 (mkSpan 1 1)
-- Just (Span 1 1)
--
-- The span of \"C\" is shifted by 3 characters.
--
-- >>> mapDiff d0 (mkSpan 4 1)
-- Just (Span 7 1)
-- >>> mapReplace r0 (mkSpan 4 1)
-- Just (Span 7 1)
-- >>> comapDiff d0 (mkSpan 7 1)
-- Just (Span 4 1)
-- >>> comapReplace r0 (mkSpan 7 1)
-- Just (Span 4 1)
--
-- The span of "ac" overlaps with the replacement, so the mapping is undefined.
--
-- >>> mapDiff d0 (mkSpan 2 2)
-- Nothing
-- >>> mapReplace r0 (mkSpan 2 2)
-- Nothing
-- >>> comapDiff d0 (mkSpan 2 5)
-- Nothing
-- >>> comapReplace r0 (mkSpan 2 5)
-- Nothing
--
-- Properties:
--
-- prop> \(FS d s) -> partialSemiInverse (mapDiff d) (comapDiff d) s
-- prop> \(FS d s) -> partialSemiInverse (comapDiff d) (mapDiff d) s
mapDiff :: Diff -> Span -> Maybe Span
mapDiff = mapDiff_ Cov

-- | Translate a span in the target of a diff to a span in its source.
-- @Nothing@ if the span overlaps with a replacement.
--
-- See also 'mapDiff'.
comapDiff :: Diff -> Span -> Maybe Span
comapDiff = mapDiff_ Contrav

data Variance = Cov | Contrav

srcV :: Variance -> Replace -> Span
srcV Cov = unsafeSrc
srcV Contrav = unsafeTgt

signV :: Variance -> Int -> Int
signV Cov = id
signV Contrav = negate

mapDiff_ :: Variance -> Diff -> Span -> Maybe Span
mapDiff_ v (Diff d) i = case FT.search (\r1 _ -> r1 `notPrecedes_` i) d of
  FT.Position d1 s _ | j `precedes` srcV v s -> Just i'
                     | otherwise -> Nothing
    where i' = shift (signV v (deltaDiff (Diff d1))) i
          j = case v of Cov -> i ; Contrav -> i'
  FT.OnLeft -> Just i
  FT.OnRight -> Just (shift (signV v (deltaDiff (Diff d))) i)
  FT.Nowhere -> error "Broken invariant"
  where
    notPrecedes_ Nothing _ = False
    notPrecedes_ (Just r1) i1 = not (srcV v r1 `precedes'` i1)

-- | Translate a span in the source of a replacement to a span in its target.
-- @Nothing@ if the span overlaps with the replacement.
--
-- See also 'mapDiff'.
--
-- Properties:
--
-- prop> \r -> partialSemiInverse (comapReplace r) (mapReplace r)
-- prop> \r -> partialSemiInverse (mapReplace r) (comapReplace r)
mapReplace :: Replace -> Span -> Maybe Span
mapReplace = mapReplace_ Cov

-- | Translate a span in the target of a replacement to a span in its source.
-- @Nothing@ if the span overlaps with the replacement.
--
-- See also 'mapDiff'.
comapReplace :: Replace -> Span -> Maybe Span
comapReplace = mapReplace_ Contrav

mapReplace_ :: Variance -> Replace -> Span -> Maybe Span
mapReplace_ v r i
  | isEmpty r || i `precedes` srcV v r = Just i
  | srcV v r `precedes'` i = Just (shift (signV v (deltaReplace r)) i)
  | otherwise = Nothing
