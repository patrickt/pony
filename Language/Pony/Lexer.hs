module Language.Pony.Lexer (lexer, lexToken) where
  
  import Control.Monad.Identity
  import Text.Parsec
  import Text.Parsec.Token
  import Text.Parsec.Language
  
  lexer :: GenTokenParser String s Identity
  lexer = makeTokenParser $ javaStyle 
      { commentLine = "//"
      , commentStart = "/*"
      , commentEnd = "*/"
      , nestedComments = True
      , reservedNames = ["int", "const", "char", "return"]
      , reservedOpNames = ["*", ";"] } 
  
  lexToken a = a lexer
