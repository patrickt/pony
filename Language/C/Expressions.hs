module Language.C.Expressions
  where 
  
  import Control.Monad
  import Debug.Trace
  import Language.C.Parser
  import Language.C.AST
  import {-# SOURCE #-} Language.C.Declarations 
  import qualified Language.C.Lexer as L
  import Text.Parsec.Expr
  
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
    let logicTable' = logicTable ++ [map mkInfixL (logicalOps st)]
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
    , [Postfix ternaryOp] ]
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
  
  mkInfixL name = Infix (L.reservedOp name >> return (BinaryOp name)) AssocLeft
  
  builtinExpression :: Parser CExpr
  builtinExpression = CBuiltin <$> builtinVaArg
  
  builtinVaArg :: Parser BuiltinExpr
  builtinVaArg = do
    L.reserved "__builtin_va_arg"
    L.symbol "("
    expr <- expression
    L.comma
    typ <- typeName
    L.symbol ")"
    return $ BuiltinVaArg expr typ
  
  castExpression :: Parser CExpr
  castExpression = do
	  types <- many $ try (L.parens typeName)
	  expr <- unaryExpression
	  return $ foldl (flip Cast) expr types
  
  sizeofExpr = pure UnaryOp <*> pure "sizeof" <*> (L.reservedOp "sizeof" *> unaryExpression)
  sizeofType = pure SizeOfType <*> (L.reservedOp "sizeof" *> L.parens typeName)
  
  prefixInc = pure UnaryOp <*> pure "++" <*> (L.reservedOp "++" *> unaryExpression)
  prefixDec = pure UnaryOp <*> pure "--" <*> (L.reservedOp "--" *> unaryExpression)
  
  unaryExpression = try sizeofExpr <|> sizeofType <|> prefixInc <|> prefixDec <|> unaryOperator
  
  unaryOperator :: Parser CExpr
  unaryOperator = do
    c <- many $ oneOf "&!*+-~!"
    -- FIXME: this is technically wrong, it should be `e <- castExpression`, but that's left-recursive if no casts are found.
    -- there are ways around this - `chainl` and such - but until this actually shows up in code as being a problem, 
    -- I'm going to leave it as is.
    e <- postfixExpression
    let c' = [ [a] | a <- c]
    return $ foldr UnaryOp e c'
  
  postfixExpression :: Parser CExpr
  postfixExpression = do
    e <- primaryExpression
    r <- many $ choice [ try arrow, dot, call, index, try increment, try decrement ]
    L.whiteSpace
    return (foldl translate e r)
    where
      translate a b = b a
      increment = do
        string "++"
        return $ UnaryOp "++ post"
      decrement = do
        string "--"
        return $ UnaryOp "-- post"
      index = do
        char '[' >> L.whiteSpace
        idx <- expression
        L.whiteSpace >> char ']'
        return $ \x -> Index x idx
      call = do
        char '(' >> L.whiteSpace
        args <- L.commaSep expression
        L.whiteSpace >> char ')'
        return $ \x -> Call x args
      dot = do
        ident <- (char '.' *> identifier)
        return $ \x -> BinaryOp "." x ident
      arrow = do
        ident <- (string "->" *> identifier)
        return $ \x -> BinaryOp "->" x ident

  primaryExpression :: Parser CExpr
  primaryExpression = choice
    [ builtinExpression
    , identifier
    , constant
    , stringLiteral
    , L.parens expression 
    ]
  
  identifier :: Parser CExpr
  identifier = Identifier <$> L.identifier 
  
  constant :: Parser CExpr
  constant = Constant <$> choice [ try float, integer, charLiteral ]
                                 
  stringLiteral :: Parser CExpr
  stringLiteral = Constant <$> CString <$> concat `liftM` (L.stringLiteral `sepBy1` L.whiteSpace)
  
  -- remember, kids, <$> is an infix synonym for fmap.
  -- TODO: The definition for integer suffixes is pretty gauche
  integer, charLiteral, float :: Parser CLiteral
  integer       = CInteger <$> L.natural <* many (oneOf "uUlL") <* L.whiteSpace
  charLiteral   = CChar    <$> L.charLiteral
  float         = CFloat   <$> L.float <* optional (oneOf "flFL") <* L.whiteSpace
  
  
