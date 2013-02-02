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
  
  import Control.Monad.State
  import Data.List (foldl')
  import Data.Monoid
  import Language.C99.Parser
  import Language.C99.AST
  import Language.C99.Literals
  import {-# SOURCE #-} Language.C99.Declarations 
  import qualified Language.C99.Lexer as L
  import Text.Parsec.Expr
  
  expression :: Parser CExpr
  expression = chainl1 assignmentExpression (Comma <$ L.comma) <?> "C expression"
  
  assignmentExpression :: Parser CExpr
  assignmentExpression = try assign <|> constantExpression where 
    assign = ((flip BinaryOp) <$> unaryExpression <*> assignmentOperator <*> assignmentExpression)
    assignmentOperator = choice $ L.symbol <$> ["=", "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", "&=", "^=", "|="]
  
  constantExpression :: Parser CExpr
  constantExpression = choice [try ternaryExpression, logicalOrExpression]
  
  -- TODO: clean this up
  
  binaryOpFor :: String -> Parser (CExpr -> CExpr -> CExpr)
  binaryOpFor str = BinaryOp <$> L.symbol str
  
  binLeft :: String -> Parser CExpr -> Parser CExpr
  binLeft str next = chainl1 next op where op = binaryOpFor str
  
  logicalOrExpression :: Parser CExpr
  logicalOrExpression = binLeft "||" logicalAndExpression
  logicalAndExpression = binLeft "&&" inclusiveOrExpression
  inclusiveOrExpression = binLeft "|" exclusiveOrExpression
  exclusiveOrExpression = binLeft "^" andExpression
  andExpression = binLeft "&" equalityExpression
  equalityExpression = chainl1 relationalExpression op where op = choice $ binaryOpFor <$> ["==", "!="]
  relationalExpression = chainl1 shiftExpression op where op = choice $ binaryOpFor <$> ["<=", ">=", "<", ">"]
  shiftExpression = chainl1 additiveExpression op where op = choice $ binaryOpFor <$> ["<<", ">>"]
  additiveExpression = chainl1 multiplicativeExpression op where op = choice $ binaryOpFor <$> ["+", "-"]
  multiplicativeExpression = chainl1 castExpression op where op = choice $ binaryOpFor <$> ["*", "/", "%"]
  
  -- end cleanup
  
  ternaryExpression :: Parser CExpr
  ternaryExpression = TernaryOp <$> logicalOrExpression <*> (L.symbol "?" *> expression) <*> (L.symbol ":" *> logicalOrExpression)
  
  builtinExpression :: Parser CExpr
  builtinExpression = CBuiltin <$> builtinVaArg
  
  builtinVaArg :: Parser CBuiltinExpr
  builtinVaArg = BuiltinVaArg <$> (L.reserved "__builtin_va_arg" *> L.parens expression)<*> typeName
  
  castExpression :: Parser CExpr
  castExpression = unwrap <$> (CCast <$> (many $ L.parens typeName) <*> unaryExpression) where
    unwrap (CCast [] x) = x
    unwrap x = x
  
  sizeofExpr = UnaryOp <$> L.reservedOp' "sizeof" <*> unaryExpression
  sizeofType = SizeOfType <$> (L.reservedOp "sizeof" *> L.parens typeName)
  
  
  
  unaryExpression :: Parser CExpr
  unaryExpression = choice [ postfixExpression
                           , UnaryOp <$> L.reservedOp' "++" <*> unaryExpression
                           , UnaryOp <$> L.reservedOp' "--" <*> unaryExpression
                           , UnaryOp <$> unaryOperator   <*> castExpression
                           , try sizeofExpr
                           , try sizeofType ] <?> "unary expression"
                           
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
  
  
