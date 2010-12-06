module Main where
  
  import Control.Monad.Error
  import Data.ConfigFile
  import Data.Generics
  import Language.C.Parser
  import Language.C.Expressions
  import Language.C.Functions
  import System.Console.CmdArgs
  import System.Environment
  
  forceRight :: Either a b -> b
  forceRight (Right r) = r
  forceLeft (Left _) = error "forceRight"
  
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
       return (val :: String)
    let inrnls = Internals {
      typedefs = [],
      newOperators = [forceRight rv]
    }
    result <- parseFromFileCustom preprocessedC input inrnls
    case result of
      (Left parseError) -> writeFile output (show parseError)
      Right externs -> writeFile output (show externs)
    
  main :: IO ()
  main = cmdArgs ponyOptions >>= parsePony
