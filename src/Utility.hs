{- Utility
By Gregory W. Schwartz

Collects functions pertaining to general helpers for the program.
-}

{-# LANGUAGE BangPatterns #-}

module Utility
    ( newickSplitters
    ) where

-- Standard

-- Cabal
import qualified Data.Text as T

-- Local
import Types

-- | Get the separator and field info for getting labels from Newick trees
newickSplitters :: (String, Int) -> (Separator, Field)
newickSplitters (!x, !y) = (Separator . T.pack $ x, Field y)

