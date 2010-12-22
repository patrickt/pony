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
  expression = buildExpressionParser assignTable constantExpression <?> "C expression"
  
  constantExpression :: Parser CExpr
  constantExpression = do
    st <- getState
    let ops = newOperators st
    let arithTable' = arithTable ++ [map mkInfixL ops]
    buildChainedParser [ (arithTable', "arithmetic expression")
                       , (compTable, "comparative expression")
                       , (bitwiseTable, "bitwise operation")
                       , (logicTable, "logical operation")
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
  
  castExpression :: Parser CExpr
  castExpression = do
    types <- many $ L.parens typeName
    expr <- unaryExpression
    return $ foldl (flip Cast) expr types

  unaryExpression = undefined
  
  -- TODO: This is terrible, and I should be ashamed of myself.
  data PostfixOp 
    = Brackets CExpr
    | Parens [CExpr]
    | Dot CExpr
    | Arrow CExpr
    | Increment
    | Decrement
    deriving (Show, Eq)
  
  postfixExpression :: Parser CExpr
  postfixExpression = do
    e <- primaryExpression
    r <- many $ choice [ Brackets <$> L.brackets expression
                       , Parens <$> L.parens (many expression) 
                       , Dot <$> (L.dot >> identifier)
                       , Arrow <$> (L.arrow >> identifier)
                       , (L.reservedOp "++" >> L.whiteSpace >> return Increment)
                       , (L.reservedOp "--" >> L.whiteSpace >> return Decrement)]
    (return $ foldl translate e r) <?> "postfix expression"
    where
      translate :: CExpr -> PostfixOp -> CExpr
      translate a (Brackets b) = Index a b
      translate a (Parens es) = Call a es
      translate a (Dot ident) = BinaryOp "." a ident
      translate a (Arrow ident) = BinaryOp "->" a ident
      translate a Increment = UnaryOp "post++" a
      translate a Decrement = UnaryOp "post--" a
  
  

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
  constant = Constant <$> choice [ try float, integer , charLiteral ]
                                 
  stringLiteral :: Parser CExpr
  stringLiteral = Constant <$> CString <$> concat `liftM` (L.stringLiteral `sepBy` L.whiteSpace)
  
  -- remember, kids, <$> is an infix synonym for fmap.
  -- TODO: The definition for integer suffixes is pretty gauche
  integer, charLiteral, float :: Parser CLiteral
  integer       = CInteger <$> L.integer <* many (oneOf "uUlL") <* L.whiteSpace
  charLiteral   = CChar    <$> L.charLiteral
  float         = CFloat   <$> L.float <* optional (oneOf "flFL") <* L.whiteSpace
  
  
