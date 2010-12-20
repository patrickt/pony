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
  
  -- instead of taking tuples, there should be an ADT that has a precedence table, a unique id, and a name for error messages
  expression :: Parser CExpr
  expression = buildChainedParser [ (assignTable, "assignment expression") ] constantExpression <?> "C expression"
  
  constantExpression :: Parser CExpr
  constantExpression = do
    st <- getState
    let ops = newOperators st
    let arithTable' = arithTable ++ [map mkInfixL ops]
    buildChainedParser [ (unaryTable, "unary expression")
                       , (castTable, "cast expression")
                       , (arithTable', "arithmetic expression")
                       , (compTable, "comparative expression")
                       , (bitwiseTable, "bitwise operation")
                       , (logicTable, "logical operation")
                       ] postfixExpression <?> "constant expression"

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
  
  castTable = 
    [ [ Prefix cast ]]
    where cast = pure Cast <*> (try $ L.parens typeName)
  
  -- BUG: the unary operators are applied on cast-expressions, not other unary expressions
  unaryTable = 
    [ [ mkPrefix "++" ]
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
      index       = pure (flip Index) <*> L.brackets expression
      call        = pure (flip Call) <*> (L.parens $ L.commaSep expression)
      dotAccess   = pure (flip $ BinaryOp ".") <*> (L.dot *> expression)
      arrowAccess = pure (flip $ BinaryOp "->") <*> (L.arrow *> expression)
      increment   = L.reservedOp "++" >> return (UnaryOp "post ++")
      decrement   = L.reservedOp "--" >> return (UnaryOp "post --")
  
  postfixExpression :: Parser CExpr
  postfixExpression = buildExpressionParser postfixTable (sizeOfType <|> primaryExpression) <?> "postfix expression"
  
  -- sizeOfType cannot fit into the operator table, since type names are not expressions.
  sizeOfType :: Parser CExpr
  sizeOfType = pure SizeOfType <*> (L.reserved "sizeof" *> L.parens typeName)
  
  primaryExpression :: Parser CExpr
  primaryExpression = choice
    [ identifier
    , constant
    , stringLiteral
    , L.parens expression 
    ]
  
  identifier :: Parser CExpr
  identifier = Identifier <$> L.identifier 
  
  constant :: Parser CExpr
  constant = Constant <$> choice [ try float
                                 , integer
                                 , charLiteral 
                                 ]
                                 
  stringLiteral :: Parser CExpr
  stringLiteral = Constant <$> CString <$> concat `liftM` (L.stringLiteral `sepBy` L.whiteSpace)
  
  -- remember, kids, <$> is an infix synonym for fmap.
  integer, charLiteral, float :: Parser CLiteral
  integer       = CInteger <$> L.integer <* many (oneOf "uUlL") <* L.whiteSpace
  charLiteral   = CChar    <$> L.charLiteral
  float         = CFloat   <$> L.float <* (optional (oneOf "flFL"))
  
  
