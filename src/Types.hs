-- Types module
-- By Gregory W. Schwartz
--
-- Collects all application specific types

{-# LANGUAGE DeriveGeneric #-}

module Types where

-- Standard
import GHC.Generics
import qualified Data.Sequence as Seq
import Data.Tree

-- Cabal
import qualified Data.Text as T
import Data.Aeson

-- Algebraic
data NodeLabel = NodeLabel { nodeID     :: !T.Text
                           , nodeLabels :: !Labels
                           } deriving (Generic, Eq, Ord, Read, Show)

data Format = Haskell | JSON | Lineage T.Text deriving (Read)

data Exclusivity = Exclusive | AllExclusive | Majority deriving (Read)

-- Simple
type Label = T.Text

-- Advanced
type Labels = Seq.Seq Label

instance FromJSON NodeLabel
