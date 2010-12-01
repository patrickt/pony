module Main where
  
  import Data.Generics
  import Language.C.Parser
  import Language.C.Functions
  import System.Console.CmdArgs
  import System.Environment
  
  -- TODO: allow for multiple inputs (input :: [FilePath])
  data PonyOptions = PonyOptions {
    output :: FilePath,
    input :: FilePath
  } deriving (Show, Typeable, Data)
  
  options :: PonyOptions
  options = PonyOptions {
    output = 
      "/dev/stdout" 
        &= typFile 
        &= help "File to which C code will be written" 
        &= opt "stdout",
    input = 
      def 
        &= typFile
        &= argPos 0 
  } &= program "pony"
    &= summary "pony 0.0.1, (c) Patrick Thomson 2010"
  
  
  parsePony :: PonyOptions -> IO ()
  parsePony PonyOptions { output, input } = do
    result <- parseFromFile preprocessedC input
    case result of
      (Left parseError) -> writeFile output (show parseError)
      Right externs -> writeFile output (show externs)
    
  main :: IO ()
  main = cmdArgs options >>= parsePony
