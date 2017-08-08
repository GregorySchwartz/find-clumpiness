{- LineageConvert
By Gregory W. Schwartz

Collects functions pertaining to converting the lineage format from the
clash database to the workable tree
-}

{-# LANGUAGE OverloadedStrings #-}

module LineageConvert
    ( lineageToTree
    , decodeLineageTree
    , getLineageTree
    ) where

-- Standard
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.Tree
import qualified Data.HashMap.Strict as Hash

-- Cabal
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text as T
import Math.TreeFun.Types
import Math.TreeFun.Tree
import Data.Aeson
import Data.Aeson.Types

-- Local
import Types

-- | Convert a lineage format into the workable tree format
lineageToTree :: Label -> Object -> Tree NodeLabel
lineageToTree label object = Node { rootLabel = getNodeLabel label object
                                  , subForest = map
                                                (lineageToTree label)
                                              . getChildren
                                              $ object
                                  }

-- | Get the NodeLabel of a node
getNodeLabel :: Label -> Object -> NodeLabel
getNodeLabel label object = NodeLabel { nodeID = getNodeID object
                                      , nodeLabels = getLabel label object
                                      }

-- | Get the NodeLabel of a node
getNodeID :: Object -> T.Text
getNodeID object = T.intercalate "_"
                 . (\(Object x) -> Hash.keys x)
                 . either error id
                 . flip parseEither object $ \obj -> do
                    info   <- obj .: "data"
                    seqIDs <- info .: "seq_ids"
                    return seqIDs

-- | Get the label of the node
getLabel :: Label -> Object -> Labels
getLabel label object = Seq.fromList
                      . V.toList
                      . either error id
                      . flip parseEither object $ \obj -> do
                            info   <- obj .: "data"
                            labels <- info .: label
                            return labels

-- | Get the children of a node
getChildren :: Object -> [Object]
getChildren object = either error id
                   . flip parseEither object $ \obj -> do
                        children <- obj .: "children"
                        return children

-- | Get the generic AST from the file
decodeLineageTree :: C.ByteString -> Object
decodeLineageTree contents = fromMaybe
                             (error "Input is not a JSON object")
                             (decode contents :: Maybe Object)

-- | Get the lineage tree from a generic AST
getLineageTree :: Label -> Object -> Tree NodeLabel
getLineageTree label object = either error (lineageToTree label)
                            . flip parseEither object $ \obj -> do
                                germTree <- obj .: "tree"
                                tree <- germTree .: "children"
                                return . rootCheck tree $ germTree
  where
    -- Get the first branch point (sometimes there are additional nodes
    -- right after the root for lineages that bypass the no root rule).
    rootCheck [tree] _ = tree
    rootCheck _ tree   = tree
