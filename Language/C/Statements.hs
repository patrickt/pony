module Language.C.Statements where
  
  import Language.C.Parser
  import Language.C.Expressions
  import Language.C.AST
  import qualified Language.C.Lexer as L
  
  statement :: Parser CStatement
  statement = undefined
  
  labeledStmt :: Parser CStatement
  labeledStmt = undefined
  
  compoundStmt :: Parser CStatement
  compoundStmt = undefined
  
  expressionStmt :: Parser CStatement
  expressionStmt = do
    expr <- optionMaybe expression
    L.semi
    return $ case expr of
      (Just e) -> ExpressionStmt e
      Nothing -> EmptyStmt
  
  selectionStmt :: Parser CStatement
  selectionStmt = undefined
  
  iterationStmt :: Parser CStatement
  iterationStmt = undefined
  
  jumpStmt :: Parser CStatement
  jumpStmt = undefined