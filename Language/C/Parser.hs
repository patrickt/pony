module Language.C.Parser 
  ( module Text.Parsec
  , module Control.Applicative
  , Parser
  , Internals(..)
  , mkInternals
  , addTypeDef
  , parseTest
  , parseTestCustom
  , parseFromFile
  , parseFromFileCustom
  , preprocessAndParse
  )
  where
  
  import System.Cmd
  import Text.Printf
  import Control.Applicative
  import Text.Parsec hiding (parseTest, (<|>), many, optional)
  import Text.Parsec.String hiding (Parser, parseFromFile)
  import Language.C.AST
  
  data Internals = Internals 
    { typedefs :: [(String, TypeSpecifier)]
    , newOperators :: [String]
    }
  
  mkInternals :: Internals
  mkInternals = Internals { typedefs = []
                          , newOperators = [] 
                          }
  
  addTypeDef :: String -> Specifier -> Internals -> Internals
  addTypeDef name (TSpec typ) record = record { typedefs = typedefs record ++ [(name, typ)]}
  
  type Parser = GenParser Char Internals
  
  preprocessAndParse p loc i = do
    let preCmd = printf "/usr/bin/clang -E %s | grep \"^[^#]\" > ./ponytmp" loc :: String 
    system preCmd
    parseFromFileCustom p "./ponytmp" i
  
  parseTest p s = 
    case (runParser p mkInternals "" s) of
      (Left error) -> print error
      (Right a) -> print a
  
  parseTestCustom p s i = 
    case (runParser p i "" s) of
      (Left error) -> print error
      (Right a) -> print a

  parseFromFile p loc = do
    str <- readFile loc
    return $ runParser p mkInternals loc str
  
  parseFromFileCustom p loc internals = do
    str <- readFile loc
    return $ runParser p internals loc str
  
  