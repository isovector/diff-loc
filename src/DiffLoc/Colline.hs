{-# LANGUAGE
  GeneralizedNewtypeDeriving,
  TypeFamilies #-}

-- | Affine space of line-column locations.
module DiffLoc.Colline
  ( Colline(..)
  , Vallee(..)
  , Vallée
  ) where

import Data.Functor ((<&>))
import DiffLoc.Shift

-- $setup
-- >>> import Test.QuickCheck
-- >>> import DiffLoc.Shift
-- >>> import DiffLoc.Test

-- | Line and column coordinates.
--
-- The generalization over types of line and column numbers
-- frees us from any specific indexing scheme, notably whether
-- columns are zero- or one-indexed.
data Colline l c = Colline !l !c
  deriving (Eq, Ord, Show)

-- | The space between two 'Colline's.
data Vallee dl dc = Vallee !dl !dc
  deriving (Eq, Ord, Show)

-- | Il fallait le faire.
type Vallée = Vallee

-- $hidden
-- prop> (x <> y) <> z === x <> (y <> z :: Vallee)

traversee ::
  Eq dl =>
  dl ->
  (l -> dl -> l) ->
  (c -> dc -> c) ->
  (dc -> c) ->
  Colline l c -> Vallee dl dc -> Colline l c
traversee zero actL actC fromO (Colline l c) (Vallee l' c')
  | l' == zero = Colline l (c `actC` c')
  | otherwise = Colline (l `actL` l') (fromO c')

instance (Monoid l, Eq l, Semigroup c) => Semigroup (Vallee l c) where
  x <> y = descente (traversee mempty (<>) (<>) id (montee x) y)
    where
      montee :: Vallee l c -> Colline l c
      montee (Vallee l c) = Colline l c

      descente :: Colline l c -> Vallee l c
      descente (Colline l c) = Vallee l c

instance (Monoid l, Eq l, Monoid c) => Monoid (Vallee l c) where
  mempty = Vallee mempty mempty

-- $hidden
-- prop> (i .+ r) .+ s === i .+ (r <> s :: Vallee)
-- prop> i <= j ==> (i .+ (j .-. i :: Vallee)) === j
-- prop> (i .+ r) .-. i === (r :: Vallee)

instance (Affine l, Origin c) => Affine (Colline l c) where
  type Trans (Colline l c) = Vallee (Trans l) (Trans c)

  (.+) = traversee mempty (.+) (.+) ofOrigin

  Colline l c .-.? Colline l' c' = case compare l l' of
    LT -> Nothing
    EQ | c' <= c -> Vallee mempty <$> (c .-.? c')
       | otherwise -> Nothing
    GT -> (l .-.? l') <&> \dl -> Vallee dl (fromOrigin c)
    -- TODO: Tests should catch the typo replacing c with c'

instance (Origin l, Origin c) => Origin (Colline l c) where
  origin = Colline origin origin
