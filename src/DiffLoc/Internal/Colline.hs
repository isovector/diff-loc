{-# LANGUAGE
  GeneralizedNewtypeDeriving,
  TypeFamilies #-}

-- | Affine space of line-column locations.
module DiffLoc.Internal.Colline where

import DiffLoc.Internal.Shift

-- $setup
-- >>> import Test.QuickCheck
-- >>> import DiffLoc.Internal.Shift
-- >>> import DiffLoc.Internal.Test

-- | Line number.
newtype Line = Line Int deriving (Eq, Ord, Show)

-- | Column number.
newtype Col = Col Int deriving (Eq, Ord, Show)

-- | Line and column.
data Colline = Colline !Line !Col
  deriving (Eq, Ord, Show)

-- | The space between two 'Colline's.
data Vallee = Vallee !(Delta Line) !(Delta Col)
  deriving (Eq, Ord, Show)

-- $hidden
-- prop> (x <> y) <> z === x <> (y <> z :: Vallee)

instance Semigroup Vallee where
  Vallee l c <> Vallee l' c'
    | l' == Delta 0 = Vallee l (c <> c')
    | otherwise = Vallee (l <> l') c'

instance Monoid Vallee where
  mempty = Vallee (Delta 0) (Delta 0)

-- $hidden
-- prop> (i .+ r) .+ s === i .+ (r <> s :: Vallee)
-- prop> i <= j ==> (i .+ (j .-. i :: Vallee)) === j
-- prop> (i .+ r) .-. i === (r :: Vallee)

instance Affine Vallee where
  type Point Vallee = Colline

  Colline (Line l) (Col c) .+ Vallee l' c' =
    let Vallee (Delta l0) (Delta c0) = Vallee (Delta l) (Delta c) <> Vallee l' c' in
    Colline (Line l0) (Col c0)

  Colline l c .-.? Colline l' c' = case compare l l' of
    LT -> Nothing
    EQ | c' <= c -> Just (Vallee deltaZero (c `delta` c'))
       | otherwise -> Nothing
    GT -> let Col c0 = c in Just (Vallee (l `delta` l') (Delta c0))

