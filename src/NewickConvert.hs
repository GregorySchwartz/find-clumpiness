{- NewickConvert
By Gregory W. Schwartz

Collects functions pertaining to converting the Newick format to the
workable tree
-}

{-# LANGUAGE OverloadedStrings #-}

module NewickConvert
    ( newickToTree
    ) where

-- Standard
import Data.Maybe
import Data.Tree
import qualified Data.Sequence as Seq

-- Cabal
import qualified Data.List.Safe as Safe
import qualified Data.Text as T
import Biobase.Newick

-- Local
import Types

-- | Convert a newick format into the workable tree format.
newickToTree :: Separator -> Field -> NewickTree -> Tree NodeLabel
newickToTree sep field = go . getNewickTree
  where
    go (Node { rootLabel = x, subForest = ls }) =
        Node { rootLabel = NodeLabel { nodeID = label x
                                     , nodeLabels = getLabels sep field
                                     . label
                                     $ x
                                     }
             , subForest = map go ls
             }

-- | Get the label by splitting the original label by a separator and
-- choosing the 1 indexed field.
getLabels :: Separator -> Field -> T.Text -> Labels
getLabels (Separator sep) (Field field) =
    Seq.singleton . fromMaybe "" . (Safe.!! (field - 1)) . T.splitOn sep
