-- |
-- = Example
--
-- You have a diff between two versions of a file. Given a source span
-- in one version, find the corresponding span in the other version.
--
-- For example, here is a diff @d@ between a source string "abcdefgh" and a target
-- string "appcfgzzh", with deletions and insertions in the middle:
--
-- >  ab cdefg  h
-- > - b  de
-- > + pp     zz
-- >  appc  fgzzh
--
-- Diffs are represented by the type 'Diff'.
-- Only locations and lengths are recorded, not the actual characters.
--
-- >>> :{
--   let d :: Diff
--       d = addReplace (mkReplace 1 1 2)  -- at location 1, replace "b" (length 1) with "pp" (length 2)
--         $ addReplace (mkReplace 3 2 0)  -- at location 3, replace "de" with ""
--         $ addReplace (mkReplace 7 0 2)  -- at location 7, replace "" with "zz"
--         $ emptyDiff
--   -- N.B.: replacements should be inserted right to left.
-- :}
--
-- The span @s@ of "fg" in the first string starts at location 5 and has length 2.
--
-- >>> let s = mkSpan 5 2 :: Span
--
-- >  a b c d e f g h
-- > 0 1 2 3 4 5 6 7 8
-- >           ^f+g+ length 2
-- >           ^
-- >           start 5
--
-- After applying the diff, the resulting span has been shifted to location 4.
--
-- >>> mapDiff d (mkSpan 5 2)
-- Just (Span 4 2)
--
-- >  a p p c f g q q h
-- > 0 1 2 3 4 5 6 7 8 9
-- >         ^f+g+ length 2
-- >         ^
-- >         start 4
--
-- Conversely, we can map spans from the target string to the source string of the diff:
--
-- >>> comapDiff d (mkSpan 4 2)
-- Just (Span 5 2)
--
-- If part of the input span is modified by the diff, there is no
-- corresponding output span.
--
-- >>> mapDiff d (mkSpan 1 2)  -- "bc" contains "b" which is edited by the diff
-- Nothing
module DiffLoc
  ( -- * API
    -- ** Spans
    Span()
  , mkSpan
  , spanStart
  , spanLength

    -- ** Replacements
  , Replace()
  , mkReplace
  , replaceStart
  , replaceSrcLength
  , replaceTgtLength
  , isEmpty
  , mapReplace
  , comapReplace

    -- ** Diffs
  , Diff()
  , emptyDiff
  , addReplace
  , mapDiff
  , comapDiff
  ) where

import DiffLoc.Internal
