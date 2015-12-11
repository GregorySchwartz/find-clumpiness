{- TreeTransform
By Gregory W. Schwartz

Collects functions pertaining to taking a Haskell tree of (Tree Label) and
converting it to a usable SuperNodeTree
-}

{-# LANGUAGE BangPatterns, ViewPatterns #-}

module TreeTransform
    ( convertToSuperTree
    , getPropertyMap
    , innerToLeaves
    , filterExclusiveTree
    ) where

-- Standard
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Tree
import qualified Data.Foldable as F
import Data.Function (on)

-- Cabal
import qualified Data.Text as T
import Math.TreeFun.Types
import Math.TreeFun.Tree

-- Local
import Types

-- | Convert the input tree to a SuperNodeTree
convertToSuperTree :: Tree NodeLabel -> Tree (SuperNode NodeLabel)
convertToSuperTree = toSuperNodeTree SuperRoot

-- | Convert inner nodes with labels to leaves
innerToLeaves :: Tree NodeLabel -> Tree NodeLabel
innerToLeaves n@(Node { subForest = [] }) = n
innerToLeaves n@( Node { rootLabel = NodeLabel { nodeID     = x
                                               , nodeLabels = (Seq.null -> True)
                                               }
                       }
                ) = n { subForest = map innerToLeaves . subForest $ n }
innerToLeaves n@( Node { rootLabel = NodeLabel { nodeID = x, nodeLabels = y }
                       , subForest = xs
                       }
                ) =
    Node { rootLabel = NodeLabel {nodeID = T.cons 'S' x, nodeLabels = Seq.empty}
         , subForest = (n { subForest = [] }) : xs
         }

-- | Get the PropertyMap of a SuperNodeTree, ignoring nodes that have no
-- labels
getPropertyMap :: Tree (SuperNode NodeLabel) -> PropertyMap NodeLabel Label
getPropertyMap = Map.fromList
               . map (\ !x -> (myRootLabel x, nodeLabels . myRootLabel $ x))
               . filter (not . Seq.null . nodeLabels . myRootLabel)
               . leaves

-- | Change labels to be exclusive or not
filterExclusiveTree :: Exclusivity
                    -> Tree (SuperNode NodeLabel)
                    -> Tree (SuperNode NodeLabel)
filterExclusiveTree ex n =
    n { rootLabel
      = (rootLabel n) { myRootLabel
                      = (myRootLabel . rootLabel $ n) { nodeLabels
                                                      = exclusiveLabel ex
                                                      . nodeLabels
                                                      . myRootLabel
                                                      . rootLabel
                                                      $ n
                                                      }
                      }
      , subForest = map (filterExclusiveTree ex) . subForest $ n
      }

-- | Transform the labels to be exclusive, non exclusive, or majority ruled
exclusiveLabel :: Exclusivity -> Labels -> Labels
exclusiveLabel _ (Seq.null -> True) = Seq.empty
exclusiveLabel AllExclusive xs      = xs
exclusiveLabel Exclusive xs         = if Seq.length xs > 1
                                        then Seq.empty
                                        else xs
exclusiveLabel Majority xs          = Seq.singleton
                                    . fst
                                    . F.maximumBy (compare `on` snd)
                                    . Map.toList
                                    . Map.fromListWith (+)
                                    . flip zip [1,1..]
                                    . F.toList $ xs
