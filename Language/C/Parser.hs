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
  import qualified Text.Parsec.Token as Token
  import Text.Parsec.Expr
  import qualified Language.C.Lexer as Lexer
  
  expression :: Parser CExpr
  expression = buildExpressionParser table primaryExpression
  
  -- to test any of these parser, go into ghci and type "parseTest <parser> <input string>"
  
  -- POSSIBLE BUG: how can I ensure that unary operators are evaluated in correct order?
  -- POSSIBLE BUG: man, this can't be correct, can it?
  -- KNOWN BUG: the cast operation is not evaluated in the correct order. X(
  table = 
    [ [ postfix "++", postfix "--" ]
    , [ prefix "++",  prefix "--", prefix "+", prefix "-", prefix "!", prefix "~", prefix "*", prefix "&", prefix "sizeof"]
    , [ _infix "*" AssocLeft, _infix "/" AssocLeft, _infix "%" AssocLeft]
    , [ _infix "+" AssocLeft, _infix "-" AssocLeft]
    , [ _infix "<<" AssocLeft, _infix ">>" AssocLeft]
    , [ _infix "<" AssocLeft, _infix "<=" AssocLeft, _infix ">" AssocLeft, _infix ">=" AssocLeft]
    , [ _infix "==" AssocLeft, _infix "!=" AssocLeft ]  
    ] where
      postfix name = Postfix $ do
        Lexer.reservedOp name
        return $ UnaryOp name
      prefix name = Prefix $ do
        Lexer.reservedOp name
        return $ UnaryOp name
      _infix name dir = Infix ((Lexer.reservedOp name) >> (return $ BinaryOp name)) dir
  
  increment :: Parser (CExpr -> CExpr)
  increment = do
    Lexer.reservedOp "++"
    return $ UnaryOp "++"
  
  primaryExpression :: Parser CExpr
  primaryExpression = choice
    [ constant
    , Lexer.parens expression
    , try callExpression
    , try indexExpression
--  , try memberExpression
--  , try castExpression
    , identifier
    ]
  
  callExpression :: Parser CExpr
  callExpression = do
    lhs <- identifier
    rhs <- Lexer.parens $ Lexer.commaSep expression
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
  
  
  
