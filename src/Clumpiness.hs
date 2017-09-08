{- Clumpiness
By Gregory W. Schwartz

Contains an easier to use version of the clumpiness function.
-}

module Clumpiness
    ( getClumpiness
    ) where

-- Cabal
import Data.Tree (Tree)
import Math.Clumpiness.Algorithms (generateClumpMap)
import Math.Clumpiness.Types (ClumpList)

-- Local
import TreeTransform
import Types

getClumpiness :: Exclusivity -- ^ How to look at vertices with multiple labels.
              -> Bool -- ^ Whether the unique node IDs are predefined (recommended False unless you know what you are doing)
              -> Bool -- ^ Whether to look at labels in inner vertices.
              -> Tree NodeLabel
              -> ClumpList Label
getClumpiness exclusivity predefinedIDs excludeInner tree =
    generateClumpMap (const True) propertyMap superTree
  where
    superTree   = convertToSuperTree
                . filterExclusiveTree exclusivity
                . (\ x -> if excludeInner
                             then x
                             else innerToLeaves x
                  )
                . (\ x -> if predefinedIDs
                             then x
                             else addUniqueNodeIDs x
                  )
                $ tree
    propertyMap = getPropertyMap superTree
