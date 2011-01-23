module Language.C.Parser 
  ( module Text.Parsec
  , module Control.Applicative
  , Parser
  , Internals(..)
  , mkInternals
  , addTypeDef
  , parseTest
  , parseTestCustom
  , parseUnsafe
  , parseFromFile
  , parseFromFileCustom
  , preprocessAndParse
  )
  where
  
  import Control.Applicative hiding ((<|>), Const)
  import Data.Generics
  import Language.C.AST
  import System.Cmd
  import Text.Parsec hiding (parseTest, many, optional)
  import Text.Parsec.String hiding (Parser, parseFromFile)
  import Text.Printf

  
  data Internals = Internals 
    { typedefs :: [(String, CDeclaration)]
    , newOperators :: [String]
    }
  
  mkInternals :: Internals
  mkInternals = Internals { typedefs = []
                          , newOperators = [] 
                          }
  
  addTypeDef :: String -> CDeclaration -> Internals -> Internals
  addTypeDef name decl record = record { typedefs = typedefs record ++ [(name, decl)]}
  
  type Parser = GenParser Char Internals
  
  preprocessAndParse p loc i = do
    let preCmd = printf "/usr/bin/clang -U __BLOCKS__ -E %s | grep \"^[^#]\" > ./ponytmp" loc :: String 
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
      
  parseUnsafe p s = 
    case (runParser p mkInternals "" s) of
      (Left e) -> error $ show e
      (Right a) -> a

  parseFromFile p loc = do
    str <- readFile loc
    return $ runParser p mkInternals loc str
  
  parseFromFileCustom p loc internals = do
    str <- readFile loc
    return $ runParser p internals loc str
  
  