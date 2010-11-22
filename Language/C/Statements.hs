module Language.C.Statements where
  
  import Language.C.Parser
  import Language.C.Expressions
  import Language.C.AST
  import qualified Language.C.Lexer as L
  
  statement :: Parser CStatement
  statement = try labeledStmt <|> expressionStmt
  
  labeledStmt :: Parser CStatement
  labeledStmt = choice [ caseStmt, defaultStmt, labelStmt ] where
    caseStmt = do 
      L.reserved "case"
      e <- expression
      L.colon
      s <- statement
      return $ CaseStmt e s
    defaultStmt = do
      L.reserved "default" >> L.colon
      DefaultStmt <$> statement
    labelStmt = do
      i <- L.identifier
      L.colon
      LabeledStmt i <$> statement
  
  compoundStmt :: Parser CStatement
  compoundStmt = L.braces $ do
    args <- many expressionStmt
    return $ CompoundStmt args
  
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
    