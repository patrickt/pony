module Main where
  
  import Data.Generics
  import Language.C
  import Language.Pony.Transformations
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
    result <- parseCFilePre input
    case result of
      Left parseError -> print "error!"
      Right translUnit -> do
        let firstStage = everywhere helloTransform translUnit
        let nextStage = everywhere mallocTransform firstStage
        let cCode = pretty nextStage  
        writeFile output (show cCode)
    
  main :: IO ()
  main = cmdArgs options >>= parsePony
