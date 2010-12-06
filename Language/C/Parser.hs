module Language.C.Parser 
  ( module Text.Parsec
  , module Control.Applicative
  , Parser
  , Internals(..)
  , mkInternals
  , addTypeDef
  , typedefs
  , parseTest
  , parseTestCustom
  , parseFromFile
  , parseFromFileCustom
  )
  where
  
  import Control.Applicative
  import Text.Parsec hiding (parseTest, (<|>), many, optional)
  import Text.Parsec.String hiding (Parser, parseFromFile)
  import Language.C.AST
  
  data Internals = Internals 
    { typedefs :: [(String, TypeSpecifier)]
    , newOperators :: [String]
    }
  
  mkInternals :: Internals
  mkInternals = Internals { typedefs = [("uint64_t", TLong)], newOperators = [] }
  
  addTypeDef :: String -> Specifier -> Internals -> Internals
  addTypeDef name (TSpec typ) record = record { typedefs = typedefs record ++ [(name, typ)]}
  
  type Parser = GenParser Char Internals
  
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
  
  