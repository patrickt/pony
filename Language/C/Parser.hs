module Language.C.Parser where
  
  import Text.Parsec
  import Text.Parsec.String
  import Language.C.AST
  import Language.C.Lexer as Lexer
  
  constant :: Parser CExpr
  constant = choice [integer, character, float, string] >>= return . Constant
  
  integer, character, float, string :: Parser CLiteral
  integer   = Lexer.integer >>= return . CInteger
  character = Lexer.charLiteral >>= return . CChar
  float     = Lexer.float >>= return . CFloat
  string    = Lexer.stringLiteral >>= return . CString
  
  
  
