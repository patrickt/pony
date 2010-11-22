module Language.C.Parser 
  ( module Text.Parsec
  , module Control.Applicative
  , Parser
  , Internals
  , typedefs
  , parseTest
  )
  where
  
  import Control.Applicative ((<$>))
  import Text.Parsec hiding (parseTest)
  import Text.Parsec.String hiding (Parser)
  import Language.C.AST
  
  data Internals = Internals {
    typedefs :: [(String, TypeSpecifier)]
  }
  
  mkInternals :: Internals
  mkInternals = Internals { typedefs = [("uint64_t", TLong)] }
  
  type Parser = GenParser Char Internals 
  
  parseTest p s = 
    case (runParser p mkInternals "" s) of
      (Left error) -> print error
      (Right a) -> print a
