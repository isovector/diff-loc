module DiffLoc
  ( -- * Spans
    Span()
  , mkSpan
  , spanStart
  , spanLength

    -- * Replacements
  , Replace()
  , mkReplace
  , replaceStart
  , replaceSrcLength
  , replaceTgtLength
  , isEmpty
  , mapReplace
  , comapReplace

    -- * Diffs
  , Diff()
  , emptyDiff
  , addReplace
  , mapDiff
  , comapDiff
  ) where

import DiffLoc.Internal
