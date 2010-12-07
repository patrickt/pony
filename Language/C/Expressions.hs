module Language.C.Expressions  
  ( expression
  , identifier
  , constantExpression
  , constant )
  where 
  
  import Language.C.Parser
  import Language.C.AST
  import {-# SOURCE #-} Language.C.Declarations 
  import qualified Language.C.Lexer as L
  import Text.Parsec.Expr
  
  buildChainedParser :: Stream s m t => [(OperatorTable s u m a, String)] -> ParsecT s u m a -> ParsecT s u m a
  buildChainedParser ((t,msg):ts) p = buildChainedParser ts (buildExpressionParser t p <?> msg)
  buildChainedParser [] p = p
  
  -- instead of taking tuples, there should be an ADT that has a precedence table, a unique id, and a name for error messages
  expression :: Parser CExpr
  expression = buildChainedParser [ (assignTable, "assignment expression") ] constantExpression <?> "C expression"
  
  constantExpression :: Parser CExpr
  constantExpression = do
    st <- getState
    let ops = newOperators st
    let arithTable' = if (not $ null ops) then arithTable ++ [map mkInfixL ops] else arithTable
    buildChainedParser [ (postfixTable, "postfix expression")
                                          , (unaryTable, "unary expression")
                                          , (arithTable', "arithmetic expression")
                                          , (compTable, "comparative expression")
                                          , (bitwiseTable, "bitwise operation")
                                          , (logicTable, "logical operation")
                                          ] primaryExpression <?> "constant expression"
  
  primaryExpression :: Parser CExpr
  primaryExpression = choice
    [ identifier
    , constant
    , Constant <$> stringLiteral
    , L.parens expression 
    ]
  
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
  
  -- BUG: the unary operators are applied on cast-expressions, not other unary expressions
  unaryTable = 
    [ [ Prefix cast   ]
    , [ mkPrefix "++" ]
    , [ mkPrefix "--" ]
    , [ mkPrefix "&"
      , mkPrefix "*"
      , mkPrefix "+"
      , mkPrefix "-"
      , mkPrefix "~"
      , mkPrefix "!"  ]
    , [ Prefix sizeof ]
    ] where
      mkPrefix name = Prefix $ do
        L.reservedOp name
        return $ UnaryOp name
      cast = pure Cast <*> L.parens typeName
      sizeof = do
        L.reserved "sizeof"
        return $ UnaryOp "sizeof"
  
  postfixTable = 
    [ [ Postfix index ]
    , [ Postfix call ]
    , [ Postfix dotAccess ]
    , [ Postfix arrowAccess ]
    , [ Postfix increment ]
    , [ Postfix decrement ]
    ] where
      index = do
        expr <- L.brackets expression
        return $ flip Index expr
      call = do
        exprs <- L.parens $ L.commaSep expression
        return $ flip Call exprs
      dotAccess = do
        i <- L.dot >> identifier
        return $ flip (BinaryOp ".") i
      arrowAccess = do
        i <- L.arrow >> identifier
        return $ flip (BinaryOp "->") i
      increment = L.reservedOp "++" >> return (UnaryOp "post ++")
      decrement = L.reservedOp "--" >> return (UnaryOp "post --")
  
  identifier :: Parser CExpr
  identifier = Identifier <$> L.identifier 
  
  constant :: Parser CExpr
  constant = Constant <$> choice [ try float
                                 , integer
                                 , charLiteral 
                                 ]
  
  -- remember, kids, <$> is an infix synonym for fmap.
  integer, charLiteral, float, stringLiteral :: Parser CLiteral
  integer       = CInteger <$> L.integer
  charLiteral   = CChar    <$> L.charLiteral
  float         = CFloat   <$> L.float
  stringLiteral = CString  <$> L.stringLiteral
  
