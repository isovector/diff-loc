{-# LANGUAGE
  GeneralizedNewtypeDeriving,
  TypeFamilies #-}

-- | Affine space of line-column locations.
module DiffLoc.Colline where

import DiffLoc.Shift

-- | Line number.
newtype Line = Line Int deriving (Eq, Ord, Show)

-- | Column number.
newtype Col = Col Int deriving (Eq, Ord, Show)

-- | Line and column.
data Colline = Colline !Line !Col
  deriving (Eq, Ord, Show)

-- | What's between two 'Colline's.
data Vallee = Vallee !(Delta Line) !(Delta Col)

instance Semigroup Vallee where
  Vallee l c <> Vallee l' c'
    | l' == Delta 0 = Vallee l (c <> c')
    | otherwise = Vallee (l <> l') c'

instance Affine Vallee where
  type Point Vallee = Colline

  Colline (Line l) (Col c) .+ Vallee l' c' =
    let Vallee (Delta l0) (Delta c0) = Vallee (Delta l) (Delta c) <> Vallee l' c' in
    Colline (Line l0) (Col c0)

  Colline l c .-.? Colline l' c' = case compare l l' of
    GT -> Nothing
    EQ | c <= c' -> Just (Vallee deltaZero (c' `delta` c))
       | otherwise -> Nothing
    LT -> let Col c0 = c' in Just (Vallee (l' `delta` l) (Delta c0))

