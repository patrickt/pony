module Language.C.Expressions
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
  
  import Control.Monad
  import Debug.Trace
  import Language.C.Parser
  import Language.C.AST
  import Language.C.Literals
  import {-# SOURCE #-} Language.C.Declarations 
  import qualified Language.C.Lexer as L
  import Text.Parsec.Expr
  import Data.List
  
  buildChainedParser :: Stream s m t => [(OperatorTable s u m a, String)] -> ParsecT s u m a -> ParsecT s u m a
  buildChainedParser ((t,msg):ts) p = buildChainedParser ts (buildExpressionParser t p <?> msg)
  buildChainedParser [] p = p
  
  expression :: Parser CExpr
  expression = buildExpressionParser assignTable constantExpression <?> "C expression"
  
  constantExpression :: Parser CExpr
  constantExpression = do
    st <- getState
    let arithTable' = arithTable ++ [map mkInfixL (arithmeticOps st)]
    let compTable' = compTable ++ [map mkInfixL (comparativeOps st)]
    let bitwiseTable' = bitwiseTable ++ [map mkInfixL (bitwiseOps st)]
    let logicTable' = logicTable ++ [map mkInfixR (logicalOps st)]
    buildChainedParser [ (arithTable', "arithmetic expression")
                       , (compTable', "comparative expression")
                       , (bitwiseTable', "bitwise operation")
                       , (logicTable', "logical operation")
                       ] castExpression <?> "constant expression"
  
  assignTable = 
    [ [ mkInfixL "="
      , mkInfixL "*=" 
      , mkInfixL "/="
      , mkInfixL "%="
      , mkInfixL "+="
      , mkInfixL "-=" 
      , mkInfixL "<<="
      , mkInfixL ">>="
      , mkInfixL "&="
      , mkInfixL "&="
      , mkInfixL "^="
      , mkInfixL "|=" ] ]

  logicTable =
    [ [mkInfixL "&&"]
    , [mkInfixL "||"]
    , [Postfix ternaryOp]
    , [mkInfixL "?:"] ]
    where
      ternaryOp = do
        then' <- L.reservedOp "?" >> expression
        else' <- L.reservedOp ":" >> expression
        return $ \it -> TernaryOp it then' else'

  bitwiseTable = 
    [ [mkInfixL "&"]
    , [mkInfixL "^"]
    , [mkInfixL "|"] ]

  compTable = 
    [ [ mkInfixL "<"
      , mkInfixL ">"
      , mkInfixL "<="
      , mkInfixL ">=" ]
    , [ mkInfixL "=="
      , mkInfixL "!=" ] ]

  arithTable = 
    [ [ mkInfixL "*"
      , mkInfixL "/"
      , mkInfixL "%" ]
    , [ mkInfixL "+"
      , mkInfixL "-" ]
    , [ mkInfixL "<<"
      , mkInfixL ">>" ] ]
    
  mkInfixL name = Infix (BinaryOp <$> L.reservedOp' name) AssocLeft
  mkInfixR name = Infix (BinaryOp <$> L.reservedOp' name) AssocRight
  
  builtinExpression :: Parser CExpr
  builtinExpression = CBuiltin <$> builtinVaArg
  
  builtinVaArg :: Parser BuiltinExpr
  builtinVaArg =  pure BuiltinVaArg 
              <*> (L.reserved "__builtin_va_arg" *> L.parens expression)
              <*> typeName
  
  castExpression :: Parser CExpr
  castExpression = do
    types <- many $ try (L.parens typeName)
    expr <- unaryExpression
    return (foldl (flip CCast) expr types) <?> "cast expression"
  
  sizeofExpr = pure UnaryOp <*> pure "sizeof" <*> (L.reservedOp "sizeof" *> unaryExpression)
  sizeofType = pure SizeOfType <*> (L.reservedOp "sizeof" *> L.parens typeName)
  
  unaryExpression :: Parser CExpr
  unaryExpression = choice [ try sizeofExpr
                           , sizeofType
                           , unaryOperator ] <?> "unary expression"
  
  unaryOperator :: Parser CExpr
  unaryOperator = do
    -- We can't use L.reservedOp' because it checks that its input is not a prefix of a valid operator. Ugh.
    c <- many $ choice $ L.symbol <$> ["&", "!", "++", "+", "--", "-", "~", "*"]
    -- FIXME: this is technically wrong, it should be `e <- castExpression`, but that's left-recursive if no casts are found.
    -- there are ways around this - `chainl` and such - but until this actually shows up in code as being a problem, 
    -- I'm going to leave it as is.
    e <- postfixExpression
    return $ foldr UnaryOp e c
  
  postfixExpression :: Parser CExpr
  postfixExpression = do
    e <- primaryExpression
    r <- many $ choice [ arrow, dot, call, index, try increment, try decrement ]
    L.whiteSpace
    -- This is known as a "bill fold". Get it? foldr ($)? HAHAHAHAHAHAHAHAHA.
    return $ foldr ($) e (reverse r)
    where
      increment = string "++" *> pure UnaryOp <*> pure "++ post"
      decrement = string "--" *> pure UnaryOp <*> pure "-- post"
      index = do
        idx <- L.brackets expression
        return $ \x -> Index x idx
      call = do
        args <- L.parens $ L.commaSep expression
        return $ \x -> Call x args
      dot = do
        ident <- L.dot *> identifier
        return $ \x -> BinaryOp "." x ident
      arrow = do
        ident <- L.arrow *> identifier
        return $ \x -> BinaryOp "->" x ident

  primaryExpression :: Parser CExpr
  primaryExpression = choice
    [ builtinExpression
    , identifier
    , constant
    , Constant <$> CString <$> stringLiteral
    , L.parens expression 
    ]
  
  identifier :: Parser CExpr
  identifier = Identifier <$> L.identifier <?> "identifier"
  
  constant :: Parser CExpr
  constant = Constant <$> choice [ try float, integer, charLiteral ] <?> "literal"
                                 
  stringLiteral :: Parser CStringLiteral
  stringLiteral = CStringLiteral <$> concat `liftM` (L.stringLiteral `sepBy1` L.whiteSpace) <?> "string literal"
  
  -- remember, kids, <$> is an infix synonym for fmap.
  -- TODO: The definition for integer suffixes is pretty gauche
  integer, charLiteral, float :: Parser CLiteral
  integer       = CInteger <$> L.natural <* many (oneOf "uUlL") <* L.whiteSpace
  charLiteral   = CChar    <$> L.charLiteral
  float         = CFloat   <$> L.float <* optional (oneOf "flFL") <* L.whiteSpace
  
  
