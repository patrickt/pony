module Language.C.Statements where
  
  import Language.C.Parser
  import Language.C.Expressions
  import Language.C.AST
  import qualified Language.C.Lexer as L
  
  statement :: Parser CStatement
  statement = try labeledStmt <|> expressionStmt <|> jumpStmt
  
  labeledStmt :: Parser CStatement
  labeledStmt = choice [ caseStmt, defaultStmt, labelStmt ] where
    caseStmt = do 
      e <- L.reserved "case" >> expression
      s <- L.colon >> statement
      return $ CaseStmt e s
    defaultStmt = DefaultStmt <$> (L.reserved "default" >> L.colon >> statement)
    labelStmt = do
      i <- L.identifier
      L.colon
      LabeledStmt i <$> statement
  
  compoundStmt :: Parser CStatement
  compoundStmt = L.braces $ do
    args <- many statement
    return $ CompoundStmt args
  
  expressionStmt :: Parser CStatement
  expressionStmt = do
    expr <- optionMaybe expression
    L.semi
    return $ maybe EmptyStmt ExpressionStmt expr
  
  selectionStmt :: Parser CStatement
  selectionStmt = undefined
  
  iterationStmt :: Parser CStatement
  iterationStmt = undefined
  
  jumpStmt :: Parser CStatement
  jumpStmt = choice [ goto, continue, break', return' ] where
    goto = L.reserved "goto" >> L.identifier >>= return . GotoStmt
    continue = L.reserved "continue" >> return ContinueStmt
    break' = L.reserved "break" >> return BreakStmt
    return' = L.reserved "return" >> (optionMaybe expression) >>= return . ReturnStmt
    