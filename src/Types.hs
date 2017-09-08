-- Types module
-- By Gregory W. Schwartz
--
-- Collects all application specific types

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

-- Remote
import Data.Aeson
import Data.Tree
import GHC.Generics
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Clustering.Hierarchical as H

-- Algebraic
data NodeLabel = NodeLabel { nodeID     :: !T.Text
                           , nodeLabels :: !Labels
                           } deriving (Generic, Eq, Ord, Read, Show)

data Format = JSON | RJSON | Haskell HaskellFormat | Newick | Lineage T.Text deriving (Read)

data HaskellFormat = BaseFormat | TreeFormat | DendrogramFormat deriving (Read)

data Exclusivity = Exclusive | AllExclusive | Majority deriving (Read)

newtype Separator = Separator T.Text deriving (Show)
newtype Field = Field Int deriving (Show)

-- Simple
type Label = T.Text

-- Advanced
type Labels = Seq.Seq Label

instance FromJSON NodeLabel

-- | Class of trees that can be converted to workable trees.
class WorkableTree a where
    makeWorkable :: a -> Tree NodeLabel

instance WorkableTree (Tree NodeLabel) where
    makeWorkable = id

instance WorkableTree (Tree (Seq.Seq T.Text)) where
    makeWorkable (Node { rootLabel = ls, subForest = xs}) =
        Node { rootLabel = NodeLabel { nodeID = "", nodeLabels = ls}
             , subForest = fmap makeWorkable xs
             }

instance WorkableTree (H.Dendrogram (Seq.Seq T.Text)) where
    makeWorkable (H.Leaf x) =
        Node { rootLabel = NodeLabel { nodeID = "", nodeLabels = x}
             , subForest = []
             }
    makeWorkable (H.Branch _ l r) =
        Node { rootLabel = NodeLabel { nodeID = "", nodeLabels = Seq.empty }
             , subForest = [makeWorkable l, makeWorkable r]
             }
