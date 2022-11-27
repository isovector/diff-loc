{-# LANGUAGE
  AllowAmbiguousTypes,
  DerivingStrategies,
  FlexibleContexts,
  FlexibleInstances,
  GeneralizedNewtypeDeriving,
  MultiParamTypeClasses,
  ScopedTypeVariables,
  StandaloneDeriving,
  TypeApplications,
  TypeFamilies,
  UndecidableInstances #-}
module DiffLoc.Internal.Diff where

import Data.Coerce
import Data.Maybe
import Data.FingerTree (FingerTree)
import qualified Data.FingerTree as FT

import DiffLoc.Internal.Interval
import DiffLoc.Internal.Shift

-- $setup
-- >>> import Control.Monad ((<=<))
-- >>> import Test.QuickCheck
-- >>> import DiffLoc.Internal.Shift
-- >>> import DiffLoc.Internal.Test
-- >>> import DiffLoc.Internal.Colline
-- >>> type V = Vallee
-- >>> quickCheck = quickCheckWith stdArgs{maxSuccess=3000}

-- | A diff represents a transformation from one file to another.
--
-- Example diff between "abcdefgh" and "appcfgzzh":
--
-- > source ab cdefg  h
-- >      -  b  de
-- >      +  pp     zz
-- > target appc  fgzzh
--
-- It consists of three replacements:
--
-- 1. replace "b" with "pp" at location 1, @mkReplace 1 1 2@;
-- 2. replace "de" with "" at location 3, @mkReplace 3 2 0@;
-- 3. replace "" with "zz" at location 7, @mkReplace 7 0 2@.
--
-- >>> :{
--   let d :: DiffR N
--       d = addReplace (Replace 1 1 2)  -- at location 1, replace "b" (length 1) with "pp" (length 2)
--         $ addReplace (Replace 3 2 0)  -- at location 3, replace "de" with ""
--         $ addReplace (Replace 7 0 2)  -- at location 7, replace "" with "zz"
--         $ emptyDiff
--   -- N.B.: replacements should be inserted right to left.
-- :}
--
-- Internally, a diff is a sequence of /disjoint/ and /nonempty/ replacements,
-- /ordered/ by their source locations.
-- The monoid annotation in the fingertree gives the endpoints of the replacements.
newtype Diff r = Diff (FingerTree (Maybe r) (R r))
  deriving (Eq, Show)

-- | A shorthand for the common use case.
type DiffR v = Diff (Replace v)

-- | The empty diff.
emptyDiff :: Semigroup r => Diff r
emptyDiff = Diff FT.empty

-- | A newtype to carry a 'Measured' instance.
newtype R r = R r
  deriving newtype (Eq, Show)

instance Semigroup r => FT.Measured (Maybe r) (R r) where
  measure (R r) = Just r

coshiftR' :: Shift r => Maybe r -> r -> r
coshiftR' Nothing = id
coshiftR' (Just r) = fromMaybe (error "failed to shift disjoint intervals") . coshiftR r

addReplaceL :: forall r. Shift r => r -> Diff r -> Diff r
addReplaceL r (Diff d0) = case FT.viewl d0 of
  FT.EmptyL -> Diff (FT.singleton (R r))
  R s FT.:< d | src r `distantlyPrecedes` src s -> Diff (R r FT.<| d0)
              | otherwise -> addReplaceL (r <> s) (Diff d)

-- | Add a replacement to a diff. The replacement is performed /after/ the diff.
--
-- === Properties
--
-- prop> \(r :: Replace N) -> not (isEmpty x) ==> mapDiff (addReplace r d) x == (shiftBlock r <=< mapDiff d) x
-- prop> \(r :: Replace N) -> not (isEmpty x) ==> comapDiff (addReplace r d) x == (comapDiff d <=< coshiftBlock r) x
addReplace :: forall r. Shift r => r -> Diff r -> Diff r
addReplace r (Diff d) = case FT.search (\r1 _-> r1 `notPrecedes_` r) d of
  FT.Position d1 s d2 -> coerce (d1 <>) (addReplaceL (coshiftR' (FT.measure d1) r) (Diff (s FT.<| d2)))
  FT.OnLeft -> addReplaceL r (Diff d)
  FT.OnRight -> Diff (d FT.|> R (coshiftR' (FT.measure d) r))
  FT.Nowhere -> error "Broken invariant"
  where
    notPrecedes_ Nothing _ = False
    notPrecedes_ (Just r1) i = not (tgt r1 `distantlyPrecedes` tgt i)
    -- Using distantlyPrecedes here and in addReplaceL lets us merge adjacent intervals.

-- $hidden
-- prop> \(r :: Replace V) -> not (isEmpty x) ==> mapDiff (addReplace r d) x == (shiftBlock r <=< mapDiff d) x
-- prop> \(r :: Replace V) -> not (isEmpty x) ==> comapDiff (addReplace r d) x == (comapDiff d <=< coshiftBlock r) x

-- | Translate a span in the source of a diff to a span in its target.
-- @Nothing@ if the span overlaps with a replacement.
--
-- For exaple, given the following 'Diff' (or 'Replace') from "aAacCc" to "aAabbbcCc":
--
-- > source aAa   cCc
-- >      - 
-- >      +    bbb
-- > target aAabbbcCc
--
-- >>> r0 = Replace 3 0 3 :: Replace N
-- >>> d0 = addReplace r0 emptyDiff
--
-- The span of \"A\" remains unchanged.
--
-- >>> mapDiff d0 (1 :.. 1)
-- Just (1 :.. 1)
-- >>> shiftBlock r0 (1 :.. 1)
-- Just (1 :.. 1)
-- >>> comapDiff d0 (1 :.. 1)
-- Just (1 :.. 1)
-- >>> coshiftBlock r0 (1 :.. 1)
-- Just (1 :.. 1)
--
-- The span of \"C\" is shifted by 3 characters.
--
-- >>> mapDiff d0 (4 :.. 1)
-- Just (7 :.. 1)
-- >>> shiftBlock r0 (4 :.. 1)
-- Just (7 :.. 1)
-- >>> comapDiff d0 (7 :.. 1)
-- Just (4 :.. 1)
-- >>> coshiftBlock r0 (7 :.. 1)
-- Just (4 :.. 1)
--
-- The span of "ac" overlaps with the replacement, so the mapping is undefined.
--
-- >>> mapDiff d0 (2 :.. 2)
-- Nothing
-- >>> shiftBlock r0 (2 :.. 2)
-- Nothing
-- >>> comapDiff d0 (2 :.. 5)
-- Nothing
-- >>> coshiftBlock r0 (2 :.. 5)
-- Nothing
--
-- === Properties
--
-- prop> \(FSN d s) -> not (isEmpty s) ==> partialSemiInverse (mapDiff d) (comapDiff d) s
-- prop> \(FSN d s) -> not (isEmpty s) ==> partialSemiInverse (comapDiff d) (mapDiff d) s
--
-- where @partialSemiInverse f g x@ is the property
--
-- > if   f x == Just y   -- for some y
-- > then g y == Just x
mapDiff :: Shift r => Diff r -> Block r -> Maybe (Block r)
mapDiff = mapDiff_ Cov

-- $hidden
--
-- prop> \(FSV d s) -> not (isEmpty s) ==> partialSemiInverse (mapDiff d) (comapDiff d) s
-- prop> \(FSV d s) -> not (isEmpty s) ==> partialSemiInverse (comapDiff d) (mapDiff d) s

-- | Translate a span in the target of a diff to a span in its source.
-- @Nothing@ if the span overlaps with a replacement.
--
-- See also 'mapDiff'.
comapDiff :: Shift r => Diff r -> Block r -> Maybe (Block r)
comapDiff = mapDiff_ Contrav

data Variance = Cov | Contrav

srcV :: Shift r => Variance -> r -> Block r
srcV Cov = src
srcV Contrav = tgt

shiftBlockV' :: Shift r => Variance -> Maybe r -> Block r -> Block r
shiftBlockV' _ Nothing = id
shiftBlockV' Cov (Just r) = fromMaybe (error "failed to shift disjoint intervals") . shiftBlock r
shiftBlockV' Contrav (Just r) = fromMaybe (error "failed to shift disjoint intervals") . coshiftBlock r

mapDiff_ :: forall r. Shift r => Variance -> Diff r -> Block r -> Maybe (Block r)
mapDiff_ v (Diff d) i = case FT.search (\r1 _ -> r1 `notPrecedes_` i) d of
  FT.Position d1 (R s) _
    | j `precedes` (srcV v s) -> Just i'
    | otherwise -> Nothing
    where i' = shiftBlockV' v (FT.measure d1) i
          j = case v of Cov -> i ; Contrav -> i'
  FT.OnLeft -> Just i
  FT.OnRight -> Just (shiftBlockV' v (FT.measure d) i)
  FT.Nowhere -> error "Broken invariant"
  where
    notPrecedes_ Nothing _ = False
    notPrecedes_ (Just r1) i1 = not (srcV v r1 `precedes` i1)
