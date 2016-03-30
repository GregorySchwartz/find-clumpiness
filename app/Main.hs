{- find-clumpiness
By Gregory W. Schwartz

Takes a Haskell or JSON tree and finds the relationship between labels
using the clumpiness metric
-}

-- Standard
import Data.Maybe
import Data.Tree

-- Cabal
import Options.Applicative
import Math.TreeFun.Tree
import Math.TreeFun.Types
import qualified Math.Clumpiness.Algorithms as Clump
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Aeson
import qualified Biobase.Newick as Newick

-- Local
import Types
import Utility
import NewickConvert
import LineageConvert
import TreeTransform
import Print

-- Command line arguments
data Options = Options { input            :: Maybe String
                       , inputFormat      :: Format
                       , inputExclusivity :: Exclusivity
                       , inputNewickLabel :: Maybe String
                       , output           :: Maybe String
                       }

-- Command line options
options :: Parser Options
options = Options
      <$> optional ( strOption
          ( long "input"
         <> short 'i'
         <> metavar "FILE"
         <> help "The input file containing the tree with labels"
          )
        )
      <*> option auto
          ( long "format"
         <> short 'f'
         <> metavar "[Haskell] | JSON | Lineage LABEL"
         <> value Haskell
         <> help "Whether the input tree is in Haskell, JSON, or Lineage format\
                 \, where the Lineage format needs an additional field to\
                 \ specify what to use as the label\
                 \ (for instance, Lineage tissues). The format for JSON is\
                 \ [{ \"nodeID\": \"ID\", \"nodeLabels\" [ \"Label\" ]  },\
                 \ [RECURSION]]"
          )
      <*> option auto
          ( long "exclusivity"
         <> short 'e'
         <> metavar "[Exclusive] | AllExclusive | Majority"
         <> value Exclusive
         <> help "Whether to only look at exclusive nodes (nodes with one label\
                 \ only), all nodes, or majority nodes (makes nodes exclusive\
                 \ by choosing the most abundant label. If a tie, then\
                 \ alphabetical)."
          )
      <*> optional ( strOption
          ( long "newick-label"
         <> short 'n'
         <> metavar "(SEPARATOR, FIELD)"
         <> help "In order to parse the Newick format, we need the labels to\
                 \ look at clumpiness. Thus, we split the original label by\
                 \ a separator and look at a specific field (1 indexed) in\
                 \ order to get the label."
          )
        )
      <*> optional ( strOption
          ( long "output"
         <> short 'o'
         <> metavar "FILE"
         <> help "The output file containing the heatmap of clumpiness"
          )
        )

-- | Get the tree from a Haskell format
haskellFormat :: Options -> IO (Tree NodeLabel)
haskellFormat opts = do
    contents <- case input opts of
                    Nothing  -> getContents
                    (Just x) -> readFile x
    return . read $ contents

-- | Get the tree from a JSON format
jsonFormat :: Options -> IO (Tree NodeLabel) 
jsonFormat opts = do
    contents <- case input opts of
                    Nothing  -> C.getContents
                    (Just x) -> C.readFile x
    return ((either error id . eitherDecode $ contents) :: Tree NodeLabel)

-- | Get the tree from a Newick format
newickFormat :: Options -> IO (Tree NodeLabel) 
newickFormat opts = do
    contents <- case input opts of
                    Nothing  -> T.getContents
                    (Just x) -> T.readFile x
    let (sep, field) = case inputNewickLabel opts of
                           Nothing  -> (Separator T.empty, Field 1)
                           (Just x) -> newickSplitters
                                       (read x :: (String, Int))

    return
        . newickToTree sep field
        . head
        . either error id
        . Newick.newicksFromText
        $ contents

-- | Get the tree form a lineage fromat
lineageFormat :: Options -> T.Text -> IO (Tree NodeLabel) 
lineageFormat opts x = do
    contents <- case input opts of
                    Nothing  -> C.getContents
                    (Just x) -> C.readFile x
    return . getLineageTree x . decodeLineageTree $ contents

findClumpiness :: Options -> IO ()
findClumpiness opts = do
    inputTree <- case inputFormat opts of
                    Haskell   -> haskellFormat opts
                    JSON      -> jsonFormat opts
                    Newick    -> newickFormat opts
                    Lineage x -> lineageFormat opts x
    let inputSuperTree = convertToSuperTree
                       . filterExclusiveTree (inputExclusivity opts)
                       . innerToLeaves
                       $ inputTree
        propertyMap    = getPropertyMap inputSuperTree
        clumpResult    = Clump.generateClumpMap
                         (const True)
                         propertyMap
                         inputSuperTree

    case output opts of
        Nothing  -> T.putStrLn . printClumpList $ clumpResult
        (Just x) ->  T.writeFile x . printClumpList $ clumpResult

main :: IO ()
main = execParser opts >>= findClumpiness
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Find the clumpiness of different pairs of labels in a\
                 \ tree"
     <> header "find-clumpiness, Gregory W. Schwartz" )
