{- find-clumpiness
By Gregory W. Schwartz

Takes a Haskell or JSON tree and finds the relationship between labels
using the clumpiness metric
-}

-- Standard
import Data.Maybe
import Data.Tree
import qualified Data.HashMap.Strict as Hash

-- Cabal
import Options.Applicative
import Math.TreeFun.Tree
import Math.TreeFun.Types
import qualified Math.Clumpiness.Algorithms as Clump
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Aeson
import qualified Data.Aeson.Types as AT

-- Local
import Types
import LineageConvert
import TreeTransform
import Print

-- Command line arguments
data Options = Options { input            :: Maybe String
                       , inputFormat      :: Format
                       , inputExclusivity :: Exclusivity
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
          ( long "output"
         <> short 'o'
         <> metavar "FILE"
         <> help "The output file containing the heatmap of clumpiness"
          )
        )

-- | Get the generic AST from the file
decodeLineageTree :: C.ByteString -> Object
decodeLineageTree contents = fromMaybe
                             (error "JSON is not an object")
                             (decode contents :: Maybe Object)

-- | Get the lineage tree from a generic AST
getLineageTree :: Label -> Object -> Tree NodeLabel
getLineageTree label object = either error (lineageToTree label)
                            . flip AT.parseEither object $ \obj -> do
                                germTree <- obj .: T.pack "tree"
                                tree <- germTree .: T.pack "children"
                                return . rootCheck tree $ germTree
  where
    -- Get the first branch point (sometimes there are additional nodes
    -- right after the root for lineages that bypass the no root rule).
    rootCheck [tree] _ = tree
    rootCheck _ tree   = tree

findClumpiness :: Options -> IO ()
findClumpiness opts = do
    contents <- case input opts of
                    Nothing  -> C.getContents
                    (Just x) -> C.readFile x

    let inputTree      = case inputFormat opts of
                            Haskell   -> read . C.unpack $ contents
                            JSON      -> either error id
                                       $ eitherDecode contents :: Tree NodeLabel
                            Lineage x -> getLineageTree x
                                       . decodeLineageTree
                                       $ contents
        inputSuperTree = convertToSuperTree
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
