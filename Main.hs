module Main where
  
  import Control.Monad.Error
  import Data.ConfigFile
  import Data.Generics
  import Language.C
  import Semantics.C
  import System.Console.CmdArgs
  import System.Environment
  import Text.CSV
  import Text.Printf
  import Language.Pony.CheckMalloc
  import Language.Pony.LogicalShift
  
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
    &= summary "pony 0.0.2, (c) George Washington University 2010-2011"
    
  
  getInternals [] = return emptyInternals
  getInternals ponyproj = do
    cp' <- runErrorT $ join $ liftIO $ readfile emptyCP ponyproj
    let cp = either (const emptyCP) id cp'
    ar <- runErrorT $ getOperators cp "arithmetic"
    co <- runErrorT $ getOperators cp "comparative"
    bw <- runErrorT $ getOperators cp "bitwise"
    lo <- runErrorT $ getOperators cp "logical"
    return Internals { 
        typedefs = []
      , arithmeticOps = either (const []) id ar
      , comparativeOps = either (const []) id co
      , bitwiseOps = either (const []) id bw
      , logicalOps = either (const []) id lo
      }
  
  getOperators :: (MonadError CPError m, MonadIO m) => ConfigParser -> String -> m [Field]
  getOperators cp name = do
    arith <- get cp "operators" name
    case (parseCSV ".ponyproj" (arith :: String)) of
      (Left e) -> throwError (ParseError (show e), "in CSV parsing")
      (Right csv) -> return $ head csv
  
  parsePony :: PonyOptions -> IO ()
  parsePony PonyOptions { output, input, ponyproj } = do
    inrnls <- getInternals ponyproj
    result <- preprocessAndParse preprocessedC input inrnls
    case result of
      (Left parseError) -> writeFile output (show parseError)
      Right externs -> do
        let converted = convertTranslationUnit externs
        let transformed = everywhere (checkMalloc `extT` convertLogicalShift) converted
        writeFile output (show $ pretty transformed)
    
  main :: IO ()
  main = cmdArgs ponyOptions >>= parsePony
