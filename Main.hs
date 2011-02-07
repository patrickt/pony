module Main where
  
  import Control.Monad.Error
  import Data.ConfigFile
  import Data.Generics
  import Language.C
  import Semantics.C.Conversions
  import System.Console.CmdArgs
  import System.Environment
  import Text.CSV
  import Text.Printf
  
  -- TODO: allow for multiple inputs (input :: [FilePath])
  data PonyOptions = PonyOptions {
    output :: FilePath,
    ponyproj :: FilePath,
    input :: FilePath
  } deriving (Show, Typeable, Data)
  
  ponyOptions :: PonyOptions
  ponyOptions = PonyOptions {
    output = 
      "/dev/stdout" 
        &= typFile 
        &= help "File to which C code will be written" 
        &= opt "stdout",
    ponyproj = 
      ""
        &= typFile
        &= help ".ponyproj file that specifies syntactic extensions"
        &= opt "project",
    input = 
      def 
        &= typFile
        &= argPos 0 
  } &= program "pony"
    &= summary "pony 0.0.1, (c) Patrick Thomson 2010"
    
  getOperators ponyproj = do
    cp <- join $ liftIO $ readfile emptyCP ponyproj
    arith <- get cp "operators" "arithmetic"
    case (parseCSV ponyproj (arith :: String)) of
      (Left e) -> throwError (ParseError (show e), "in CSV parsing")
      (Right csv) -> return $ head csv
  
  parsePony :: PonyOptions -> IO ()
  parsePony PonyOptions { output, input, ponyproj } = do
    rv <- runErrorT $ getOperators ponyproj
    let inrnls = Internals {
      typedefs = []
    , arithmeticOps = either (const []) id rv
    , comparativeOps = []
    , bitwiseOps = []
    , logicalOps = []
    }
    result <- preprocessAndParse preprocessedC input inrnls
    case result of
      (Left parseError) -> writeFile output (show parseError)
      Right externs -> writeFile output $ printf "Parsed %d nodes\n" (gnodecount externs)
    
  main :: IO ()
  main = cmdArgs ponyOptions >>= parsePony
