module Language.C99.Expressions

  where 
  
  import Control.Arrow ((>>>), (<<<))
  import Data.Function (on)
  import Data.Functor.Identity (Identity)
  import Data.List (groupBy)
  import Language.C99.Parser
  import Language.C99.AST
  import Language.C99.Syntax
  import Language.C99.Literals
  import Language.C99.Operators
  import {-# SOURCE #-} Language.C99.Declarations 
  import qualified Language.C99.Lexer as L
  import qualified Text.Parsec.Expr as E
  import Text.Parsec.Expr (Assoc (..))
  import qualified Text.Parsec.ByteString as P
  
  -- TODO: move this
  opt' :: Parser CSyn -> Parser CSyn
  opt' p = fromMaybe <$> pure nil' <*> optional p
  
  newtype Operator = Operator { unOperator :: (GenOperator Parser) }
  
  sname' s = name' <$> L.symbol s
  
  fbinary' = flip binary'
  
  cautiousBinaryOp s p = Operator (Infix (try $ fbinary' <$> sname' s) p AssocLeft)
  
  basicBinaryOp :: String -> Int -> Operator
  basicBinaryOp s p = Operator (Infix (fbinary' <$> sname' s) p AssocLeft)
  
  smartBinaryOp :: String -> String -> Int -> Operator  
  smartBinaryOp s s2 p = Operator (Infix (try $ fbinary' <$> (sname' s <* notFollowedBy (oneOf s2))) p AssocLeft)
  
  rBinaryOp :: String -> Int -> Operator
  rBinaryOp s p = Operator (Infix (try $ (fbinary' <$> (sname' s <* notFollowedBy (L.symbol s)))) p AssocRight)
  
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
  ternary = Operator (Infix ((flip ternary') <$> (L.symbol "?" *> opt' expression <* L.symbol ":")) 11 E.AssocRight)
  
  
  defaultOperators :: [Operator]
  defaultOperators = 
    [ logicalOr
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
    ]
  
  expression = chainl1 assignmentExpression (comma' <$ L.comma) <?> "C expression"
    
  assignmentExpression :: Parser CSyn
  assignmentExpression = try assign <|> constantExpression where 
    assign = (binary' <$> constantExpression <*> assignmentOperator <*> assignmentExpression)
    assignmentOperator = choice $ sname' <$> ["=", "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", "&=", "^=", "|="]
  
  constantExpression :: Parser CSyn
  constantExpression = do
    table <- (reverse <$> groupOps <$> operators <$> getState)
    E.buildExpressionParser table prefixExpression
  
  
  parserFromOp (Infix p _ a) = E.Infix p a
  
  groupOps x = (map parserFromOp) <$> groupBy ((==) `on` precedence) x' where x' = unOperator <$> x

  unaryOperator :: Parser CSyn
  unaryOperator = name' <$> (choice (L.symbol <$> ["&", "*", "+", "-", "~", "!", "sizeof"]))
  
  postfixOperator :: Parser (CSyn -> CSyn)
  postfixOperator = choice 
    [ index' <$$> L.brackets expression
    , call' <$$> (L.parens $ L.commaSep expression)
    , access'' <$> (name' <$> L.dot) <*> identifier
    , access'' <$> (name' <$> L.arrow) <*> identifier
    , unary' <$$> name' <$> L.reservedOp' "++"
    , unary' <$$> name' <$> L.reservedOp' "--"
    ] where access'' b c a = access' a b c
  
  postfixExpression :: Parser CSyn
  postfixExpression = do
    subject <- primaryExpression
    postfixes <- many postfixOperator
    return $ foldl (>>>) id postfixes subject
  
  prefixExpression :: Parser CSyn
  prefixExpression = foldl (<<<) id <$> many prefixOperator <*> postfixExpression
  
  prefixOperator :: Parser (CSyn -> CSyn)
  prefixOperator = choice
    [ unary' <$> name' <$> L.reservedOp' "--"
    , unary' <$> name' <$> L.reservedOp' "++"
    , cast' <$> (L.parens typeName)
    , unary' <$> unaryOperator
    ]
  
  primaryExpression :: Parser CSyn
  primaryExpression = choice
    [ identifier
    , constant
    , stringLiteral
    , paren' <$> L.parens expression 
    ]
  
  -- builtinExpression :: Parser CExpr
  -- builtinExpression = CBuiltin <$> (BuiltinVaArg <$> (L.reserved "__builtin_va_arg" *> L.parens expression) <*> typeName)
  
  identifier :: Parser CSyn
  identifier = name' <$> L.identifier <?> "identifier"
  
  constant :: Parser CSyn
  constant = choice [ try float, integer, charLiteral ] <?> "literal"
                                 
  stringLiteral :: Parser CSyn
  stringLiteral = cstr' <$> concat <$> L.stringLiteral `sepBy1` L.whiteSpace <?> "string literal"
  -- stringLiteral = CStringLiteral <$> Constant <$> CString <$> concat <$> L.stringLiteral `sepBy1` L.whiteSpace <?> "string literal"
  
  -- TODO: clean up the way we do integer/float suffixes, and perhaps carry that on to the semantic stage
  integer, charLiteral, float :: Parser CSyn
  integer       = cint'    <$> L.natural <* many (oneOf "uUlL") <* L.whiteSpace
  charLiteral   = cchar'   <$> L.charLiteral
  float         = cfloat'  <$> L.float <* optional (oneOf "flFL") <* L.whiteSpace
  
  
