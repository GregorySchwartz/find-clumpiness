{- RJSONConvert
By Gregory W. Schwartz

Collects functions pertaining to converting the JSON output of R to the workable
tree. To create the input for this program from R:

@
library(data.tree)
library(jsonlite)
hc = hclust(dist(USArrests), "ave")
tree = as.Node(as.dendrogram(hc))
toJSON(as.list(tree, mode = "explicit", unname = TRUE))
@

-}

{-# LANGUAGE OverloadedStrings #-}

module RJSONConvert
    ( rJsonToTree
    , decodeRJsonTree
    , getRJsonTree
    ) where

-- Standard
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.Tree
import Debug.Trace

-- Cabal
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text as T
import TextShow (showt)
import Math.TreeFun.Types
import Math.TreeFun.Tree
import Data.Aeson
import Data.Aeson.Types

-- Local
import Types

-- | Convert a R JSON format into the workable tree format
rJsonToTree :: Object -> Tree NodeLabel
rJsonToTree object =
    Node { rootLabel = getNodeLabel object
         , subForest = fmap rJsonToTree . getChildren $ object
         }

-- | Get the NodeLabel of a node
getNodeLabel :: Object -> NodeLabel
getNodeLabel object = do
    NodeLabel { nodeID = ""
              , nodeLabels = getLabel object
              }

-- | Get the label of the node
getLabel :: Object -> Labels
getLabel object = Seq.fromList
                . V.toList
                . either error id
                . flip parseEither object $ \obj -> do
                    labels <- obj .: "name"
                    return labels

-- | Get the children of a node
getChildren :: Object -> [Object]
getChildren object = either (const []) id
                   . flip parseEither object $ \obj -> do
                        children <- obj .: "children"
                        return children

-- | Get the generic AST from the file
decodeRJsonTree :: C.ByteString -> Object
decodeRJsonTree contents = fromMaybe
                             (error "Input is not a JSON object")
                             (decode contents :: Maybe Object)

-- | Get the lineage tree from a generic AST
getRJsonTree :: Object -> Tree NodeLabel
getRJsonTree object = rJsonToTree object
