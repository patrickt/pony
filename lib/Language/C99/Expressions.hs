module Language.C99.Expressions
  ( expression
  , constantExpression
  , builtinExpression
  , castExpression
  , postfixExpression
  , unaryExpression
  , primaryExpression
  , constant
  , identifier
  , stringLiteral
  )
  where 
  
  import Data.Function (on)
  import Data.List (groupBy)
  import Language.C99.Parser
  import Language.C99.AST
  import Language.C99.Literals
  import Language.C99.Operators
  import {-# SOURCE #-} Language.C99.Declarations 
  import qualified Language.C99.Lexer as L
  import qualified Text.Parsec.Expr as E
  
  expression :: Parser CExpr
  expression = chainl1 assignmentExpression (Comma <$ L.comma) <?> "C expression"
  
  assignmentExpression :: Parser CExpr
  assignmentExpression = try assign <|> constantExpression where 
    assign = ((flip BinaryOp) <$> unaryExpression <*> assignmentOperator <*> assignmentExpression)
    assignmentOperator = choice $ L.symbol <$> ["=", "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", "&=", "^=", "|="]
  
  constantExpression :: Parser CExpr
  constantExpression = choice [try ternaryExpression, binaryExpression]
  
  -- This is incredibly cheesy, but works around the fact that reservedOp doesn't parse
  -- constructs like "a!=*b" because it looks too far ahead.
  res op = try $ do
    L.whiteSpace
    string op
    notFollowedBy $ string op
    L.whiteSpace
    return op
  
  parserFromOp op = E.Infix (BinaryOp <$> (res $ text op)) E.AssocLeft
  
  groupOps x = (map parserFromOp) <$> groupBy ((==) `on` precedence) x
  
  binaryExpression :: Parser CExpr
  binaryExpression = do
    table <- (reverse <$> groupOps <$> operators <$> getState)
    E.buildExpressionParser table castExpression
  
  ternaryExpression :: Parser CExpr
  ternaryExpression = TernaryOp <$> binaryExpression <*> (L.symbol "?" *> expression) <*> (L.symbol ":" *> binaryExpression)
  
  builtinExpression :: Parser CExpr
  builtinExpression = CBuiltin <$> builtinVaArg
  
  builtinVaArg :: Parser CBuiltinExpr
  builtinVaArg = BuiltinVaArg <$> (L.reserved "__builtin_va_arg" *> L.parens expression)<*> typeName
  
  castExpression :: Parser CExpr
  castExpression = unwrap <$> (CCast <$> (many $ try (L.parens typeName)) <*> unaryExpression) where
    unwrap (CCast [] x) = x
    unwrap x = x

  unaryExpression :: Parser CExpr
  unaryExpression = choice [ postfixExpression
                           , UnaryOp <$> L.reservedOp' "++" <*> unaryExpression
                           , UnaryOp <$> L.reservedOp' "--" <*> unaryExpression
                           , UnaryOp <$> unaryOperator   <*> castExpression
                           , try $ UnaryOp    <$> L.reservedOp' "sizeof" <*> unaryExpression
                           , try $ SizeOfType <$> (L.reservedOp "sizeof" *> L.parens typeName) 
                           ] <?> "unary expression"
                           
  unaryOperator :: Parser String
  unaryOperator = choice $ L.symbol <$> ["&", "*", "+", "-", "~", "!"]
  
  postfixOperand :: Parser CPostfix
  postfixOperand = choice 
    [ Index         <$> L.brackets expression
    , Call          <$> (L.parens $ L.commaSep assignmentExpression)
    , MemberAccess  <$> (L.dot *> (getIdent <$> identifier))
    , PointerAccess <$> (L.arrow *> (getIdent <$> identifier))
    , PostIncrement <$ L.reservedOp "++"
    , PostDecrement <$ L.reservedOp "--"
    ]
  
  postfixExpression :: Parser CExpr
  postfixExpression = unwrap <$> (PostfixOp <$> primaryExpression <*> many postfixOperand) where
    unwrap (PostfixOp just []) = just
    unwrap x = x

  primaryExpression :: Parser CExpr
  primaryExpression = choice
    [ builtinExpression
    , identifier
    , constant
    , getExpr <$> stringLiteral
    , CParen  <$> L.parens expression 
    ]
  
  identifier :: Parser CExpr
  identifier = Identifier <$> L.identifier <?> "identifier"
  
  constant :: Parser CExpr
  constant = Constant <$> choice [ try float, integer, charLiteral ] <?> "literal"
                                 
  stringLiteral :: Parser CStringLiteral
  stringLiteral = CStringLiteral <$> Constant <$> CString <$> concat <$> L.stringLiteral `sepBy1` L.whiteSpace <?> "string literal"
  
  -- TODO: clean up the way we do integer/float suffixes, and perhaps carry that on to the semantic stage
  integer, charLiteral, float :: Parser CLiteral
  integer       = CInteger <$> L.natural <* many (oneOf "uUlL") <* L.whiteSpace
  charLiteral   = CChar    <$> L.charLiteral
  float         = CFloat   <$> L.float <* optional (oneOf "flFL") <* L.whiteSpace
  
  
