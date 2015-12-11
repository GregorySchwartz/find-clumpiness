{- Print
By Gregory W. Schwartz

Collects functions pertaining to the printing of results
-}

{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Print
    ( printClumpList
    ) where

-- Cabal
import qualified Data.Text as T
import qualified Math.Clumpiness.Types as ClumpType
import TextShow

-- Local
import Types

printClumpList :: ClumpType.ClumpList Label -> T.Text
printClumpList clumpList = T.append header body
  where
    header = "property1,property2,value\n"
    body   = T.unlines
           . map ( \(!p1, !p2, !v) -> T.intercalate "," [ p1
                                                        , p2
                                                        , showt v
                                                        ]
                 )
           $ clumpList
