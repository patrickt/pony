module Main where
  
  import Control.Monad.Error
  import Data.ConfigFile
  import Data.Generics
  import Language.C.Parser
  import Language.C.Expressions
  import Language.C.AST
  import Language.C.Functions
  import Language.C.TopLevel
  import System.Console.CmdArgs
  import System.Environment
  
  split :: String -> Char -> [String]
  split [] delim = [""]
  split (c:cs) delim
     | c == delim = "" : rest
     | otherwise = (c : head rest) : tail rest
     where
         rest = split cs delim
  
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
  
  
  parsePony :: PonyOptions -> IO ()
  parsePony PonyOptions { output, input, ponyproj } = do
    rv <- runErrorT $
     do
       cp <- join $ liftIO $ readfile emptyCP ponyproj
       val <- get cp "operators" "arithmetic"
       let v = val :: String
       return $ split v ','
    let inrnls = Internals {
      typedefs = [ ("__builtin_va_list", TVoid) 
                 , ("va_list", TVoid)
                 ]
    , newOperators = (either (const []) id rv)
    }
    result <- preprocessAndParse preprocessedC input inrnls
    case result of
      (Left parseError) -> writeFile output (show parseError)
      Right externs -> writeFile output (show externs)
    
  main :: IO ()
  main = cmdArgs ponyOptions >>= parsePony
