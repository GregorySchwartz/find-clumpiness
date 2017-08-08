{- find-clumpiness
By Gregory W. Schwartz

Takes a Haskell or JSON tree and finds the relationship between labels
using the clumpiness metric
-}

-- Standard
import Data.Maybe
import Data.Tree
import Data.Semigroup ((<>))

-- Cabal
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Aeson
import Math.TreeFun.Tree
import Math.TreeFun.Types
import qualified Math.Clumpiness.Algorithms as Clump
import qualified Biobase.Newick as Newick
import Options.Applicative

-- Local
import Types
import Utility
import NewickConvert
import RJSONConvert
import LineageConvert
import TreeTransform
import Print

-- Command line arguments
data Options = Options { input            :: Maybe String
                       , inputFormat      :: Format
                       , inputExclusivity :: Exclusivity
                       , inputNewickLabel :: Maybe String
                       , excludeInnerFlag :: Bool
                       , predefinedIDs    :: Bool
                       , output           :: Maybe String
                       }

-- Command line options
options :: Parser Options
options = Options
      <$> optional ( strOption
          ( long "input"
         <> short 'i'
         <> metavar "FILE"
         <> help "The input file containing the tree with labels (or just use stdin)"
          )
        )
      <*> option auto
          ( long "format"
         <> short 'f'
         <> metavar "[JSON] | RJSON | Haskell | Lineage LABEL"
         <> value JSON
         <> help "Whether the input tree is in JSON, RJSON, Haskell, or Lineage format.\
                 \ The format for JSON is\
                 \ [{ \"nodeID\": \"ID\", \"nodeLabels\" [ \"Label\" ]  }, [RECURSION]].\
                 \ The format for RJSON is from\
                 \ \"toJSON(as.list(as.Node(dendrogram), mode = \"explicit\", unname = TRUE))\"\
                 \ using the data.tree and jsonlite libraries,\
                 \ where \"dendrogram\" is a dendrogram object in R.\
                 \ The format for Haskell is \"Tree NodeLabel\" from this library.\
                 \ The Lineage format needs an additional field to\
                 \ specify what to use as the label\
                 \ (for instance, Lineage tissues).\
                 \ See README for more information about formats."
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
      <*> switch
          ( long "exclude-inner"
         <> short 'E'
         <> help "Do not include inner node labels in clumpiness calculation."
          )
      <*> switch
          ( long "predefined-ids"
         <> short 'p'
         <> help "Whether the node IDs are predefined. Otherwise,\
                 \ we set the unique ID of the node as Text integers\
                 \ starting at 0. Recommended to ignore this flag unless you\
                 \ know what you are doing. Ignore this flag for RJSON."
          )
      <*> optional ( strOption
          ( long "output"
         <> short 'o'
         <> metavar "FILE"
         <> help "The output file containing the heatmap of clumpiness (or just use stdout)"
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

-- | Get the tree from a lineage fromat
lineageFormat :: Options -> T.Text -> IO (Tree NodeLabel)
lineageFormat opts l = do
    contents <- case input opts of
                    Nothing  -> C.getContents
                    (Just x) -> C.readFile x
    return . getLineageTree l . decodeLineageTree $ contents

-- | Get the tree from an R JSON format
rJsonFormat :: Options -> IO (Tree NodeLabel)
rJsonFormat opts = do
    contents <- case input opts of
                    Nothing  -> C.getContents
                    (Just x) -> C.readFile x
    return . getRJsonTree . decodeRJsonTree $ contents

findClumpiness :: Options -> IO ()
findClumpiness opts = do
    inputTree <- case inputFormat opts of
                    Haskell   -> haskellFormat opts
                    JSON      -> jsonFormat opts
                    RJSON     -> rJsonFormat opts
                    Newick    -> newickFormat opts
                    Lineage x -> lineageFormat opts x
    let inputSuperTree = convertToSuperTree
                       . filterExclusiveTree (inputExclusivity opts)
                       . (\ x -> if excludeInnerFlag opts
                                    then x
                                    else innerToLeaves x
                         )
                       . (\ x -> if predefinedIDs opts
                                    then x
                                    else addUniqueNodeIDs x
                         )
                       $ inputTree
        propertyMap    = getPropertyMap inputSuperTree
        clumpResult    = Clump.generateClumpMap
                         (const True)
                         propertyMap
                         inputSuperTree

    case output opts of
        Nothing  -> T.putStr . printClumpList $ clumpResult
        (Just x) ->  T.writeFile x . printClumpList $ clumpResult

main :: IO ()
main = execParser opts >>= findClumpiness
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Find the clumpiness of different pairs of labels in a\
                 \ tree"
     <> header "find-clumpiness, Gregory W. Schwartz" )
