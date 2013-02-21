module Language.C99.Expressions
  ( expression
  , Operator (..)
  , constantExpression
  , identifier
  , constant
  , stringLiteral
  , defaultOperators
  , module Language.C99.Operators
  )
  where 
  
  import Data.Function (on)
  import Data.Functor.Identity (Identity)
  import Data.List (groupBy)
  import Language.C99.Parser
  import Language.C99.AST
  import Language.C99.Literals
  import Language.C99.Operators
  import {-# SOURCE #-} Language.C99.Declarations 
  import qualified Language.C99.Lexer as L
  import qualified Text.Parsec.Expr as E
  import Text.Parsec.Expr (Assoc (..))
  import qualified Text.Parsec.ByteString as P
  
  newtype Operator = Operator { unOperator :: (GenOperator Parser) }
  
  cautiousBinaryOp s p = Operator (Infix (try $ BinaryOp <$> L.symbol s) p AssocLeft)
  
  basicBinaryOp :: String -> Int -> Operator
  basicBinaryOp s p = Operator (Infix (BinaryOp <$> L.symbol s) p AssocLeft)
  
  smartBinaryOp :: String -> String -> Int -> Operator  
  smartBinaryOp s s2 p = Operator (Infix (BinaryOp <$> (L.symbol s <* notFollowedBy (oneOf s2))) p AssocLeft)

  rBinaryOp :: String -> Int -> Operator
  rBinaryOp s p = Operator (Infix (try $ (BinaryOp <$> (L.symbol s <* notFollowedBy (L.symbol s)))) p AssocRight)
  
  logicalOr :: Operator
  logicalOr = basicBinaryOp "||" 1
  
  logicalAnd :: Operator
  logicalAnd = basicBinaryOp "&&" 2
  
  inclusiveOr :: Operator
  inclusiveOr = smartBinaryOp "|" "|=" 3
  
  exclusiveOr :: Operator
  exclusiveOr = smartBinaryOp "^" "=" 4
  
  binaryAnd :: Operator
  binaryAnd = smartBinaryOp "&" "&=" 5
  
  equality, inequality :: Operator
  equality = cautiousBinaryOp "==" 6
  inequality = basicBinaryOp "!=" 6
  
  lessThanEq, greaterThanEq, lessThan, greaterThan :: Operator
  lessThanEq = cautiousBinaryOp "<=" 7
  greaterThanEq = cautiousBinaryOp ">=" 7
  lessThan = smartBinaryOp "<" "=<" 7
  greaterThan = smartBinaryOp ">" "=>" 7
  
  leftShift, rightShift :: Operator
  leftShift = cautiousBinaryOp "<<" 8
  rightShift = cautiousBinaryOp ">>" 8
  
  plus, minus :: Operator
  plus = smartBinaryOp "+" "=" 9
  minus = smartBinaryOp "-" "=" 9
  
  multiply, divide, modulus :: Operator
  multiply = smartBinaryOp "*" "=" 10
  divide = smartBinaryOp "-" "=" 10
  modulus = smartBinaryOp "%" "=" 10
  
  ternary :: Operator
  ternary = Operator (Infix ((flip TernaryOp2) <$> (L.symbol "?" *> optional expression <* L.symbol ":")) 11 E.AssocRight)
  
  postfixes :: Operator
  postfixes = Operator (Postfix ((flip PostfixOp) <$> some postfixOperand) 12)
  
  prefixes :: Operator
  prefixes = Operator (Prefix (PrefixOp <$> some prefixOperand) 13)
  
  defaultOperators :: [Operator]
  defaultOperators = 
    [logicalOr
    , logicalAnd
    , inclusiveOr
    , exclusiveOr
    , binaryAnd
    , equality
    , inequality
    , lessThanEq
    , greaterThanEq
    , lessThan
    , greaterThan
    , leftShift
    , rightShift
    , plus
    , minus
    , multiply
    , divide
    , modulus
    , ternary
    , postfixes
    , prefixes
    ]
  
  expression :: Parser CExpr
  expression = chainl1 assignmentExpression (Comma <$ L.comma) <?> "C expression"
  
  assignmentExpression :: Parser CExpr
  assignmentExpression = try assign <|> constantExpression where 
    assign = ((flip BinaryOp) <$> constantExpression <*> assignmentOperator <*> assignmentExpression)
    assignmentOperator = choice $ L.symbol <$> ["=", "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", "&=", "^=", "|="]
  
  constantExpression :: Parser CExpr
  constantExpression = do
    table <- (reverse <$> groupOps <$> operators <$> getState)
    E.buildExpressionParser table primaryExpression
  
  
  parserFromOp (Infix p _ a) = E.Infix p a
  parserFromOp (Prefix p _) = E.Prefix p
  parserFromOp (Postfix p _) = E.Postfix p
  
  groupOps x = (map parserFromOp) <$> groupBy ((==) `on` precedence) x' where x' = unOperator <$> x
        
  unaryOperator :: Parser String
  unaryOperator = choice $ L.symbol <$> ["&", "*", "+", "-", "~", "!", "sizeof"]
  
  postfixOperand :: Parser CPostfix
  postfixOperand = choice 
    [ Index         <$> L.brackets expression
    , Call          <$> (L.parens $ L.commaSep expression)
    , MemberAccess  <$> (L.dot *> (getIdent <$> identifier))
    , PointerAccess <$> (L.arrow *> (getIdent <$> identifier))
    , PostIncrement <$ L.reservedOp "++"
    , PostDecrement <$ L.reservedOp "--"
    ]
  
  prefixOperand :: Parser CPrefix
  prefixOperand = choice
    [ PreIncrement <$ L.reservedOp "++"
    , PreDecrement <$ L.reservedOp "--"
    , try $ Cast2 <$> L.parens typeName
    , CUnaryOp <$> unaryOperator
    ]
  
  primaryExpression :: Parser CExpr
  primaryExpression = choice
    [ builtinExpression
    , identifier
    , constant
    , getExpr <$> stringLiteral
    , CParen  <$> L.parens expression 
    ]
  
  type COperator = E.Operator ByteString Internals Identity CExpr
  
  data Op = Op { baseOperator :: COperator, prec :: Int }
  
  builtinExpression :: Parser CExpr
  builtinExpression = CBuiltin <$> (BuiltinVaArg <$> (L.reserved "__builtin_va_arg" *> L.parens expression) <*> typeName)
  
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
  
  
