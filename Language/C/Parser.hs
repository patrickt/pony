module Language.C.Parser  
  ( expression
  , primaryExpression
  , identifier
  , constant
  , integer
  , character
  , float
  , string )
  where
  
  import Text.Parsec hiding (string)
  import Text.Parsec.String
  import Language.C.AST
  import qualified Language.C.Lexer as Lexer
  
  expression :: Parser CExpr
  expression = undefined
  
  primaryExpression :: Parser CExpr
  primaryExpression = choice
    [ constant
    --, Lexer.parens expression
    , try callExpression
    , try indexExpression
    , identifier
    ]
  
  callExpression :: Parser CExpr
  callExpression = do
    lhs <- identifier
    rhs <- Lexer.parens $ Lexer.commaSep primaryExpression
    return $ Call lhs rhs
  
  indexExpression :: Parser CExpr
  indexExpression = do
    lhs <- identifier
    rhs <- Lexer.brackets $ expression
    return $ Index lhs rhs
  
  identifier :: Parser CExpr
  identifier = Lexer.identifier >>= return . Identifier
  
  constant :: Parser CExpr
  constant = choice [try float, integer, character, string] >>= return . Constant
  
  integer, character, float, string :: Parser CLiteral
  integer   = Lexer.integer >>= return . CInteger
  character = Lexer.charLiteral >>= return . CChar
  float     = Lexer.float >>= return . CFloat
  string    = Lexer.stringLiteral >>= return . CString
  
  
  
