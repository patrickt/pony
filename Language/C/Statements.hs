module Language.C.Statements where
  
  import Language.C.Parser
  import Language.C.Expressions
  import Language.C.AST
  import Language.C.Declarations
  import qualified Language.C.Lexer as L
  
{-
What this really should be is a chained parser akin to the one in Expressions.hs.

  statement :: Parser CStatement
  statement = buildChainedParser [ (compoundStmt, False) 
                                 , (selectionStmt, False)
                                 , (iterationStmt, False)
                                 , (jumpStmt, False)
                                 , (labeledStmt, True)
                                 ]
where the boolean indicates whether the use of the `try` combinator is necessary, i.e. 
whether it can consume input before failing.
-}
  
  statement :: Parser CStatement
  statement  = try labeledStmt 
            <|> compoundStmt 
            <|> jumpStmt 
            <|> expressionStmt
            <|> selectionStmt
            <|> iterationStmt
  
  labeledStmt :: Parser CStatement
  labeledStmt = choice [ caseStmt
                       , defaultStmt
                       , labelStmt
                       ] where
    caseStmt    = pure CaseStmt <*> (L.reserved "case" *> expression) <*> (L.colon *> statement)
    defaultStmt = DefaultStmt <$> (L.reserved "default" >> L.colon >> statement)
    labelStmt   = pure LabeledStmt <*> (L.identifier <* L.colon) <*> statement 
  
  compoundStmt :: Parser CStatement
  compoundStmt = L.braces $ pure CompoundStmt <*> many blockItem
  
  blockItem :: Parser BlockItem
  blockItem = try (Left <$> declaration) <|> (Right <$> statement)
  
  expressionStmt :: Parser CStatement
  expressionStmt = do
    expr <- optionMaybe expression
    L.semi
    return $ maybe EmptyStmt ExpressionStmt expr
  
  selectionStmt :: Parser CStatement
  selectionStmt = ifStmt <|> switch where
    ifStmt = pure IfStmt <*> (L.reserved "if" *> (L.parens expression))
                         <*> statement
                         <*> optional (L.reserved "else" *> statement)
    switch = pure SwitchStmt <*> L.parens expression <*> statement
  
  iterationStmt :: Parser CStatement
  iterationStmt = choice [ while
                         , doWhile
                         , try newFor
                         , oldFor 
                         ] where
    while = pure WhileStmt <*> (L.reserved "while" *> L.parens expression) <*> statement
    doWhile = pure DoWhileStmt <*> (L.reserved "do" *> statement)
                               <*> (L.reserved "while" *> (L.parens expression <* L.semi))
    oldFor = do
      L.reserved "for"
      (a, b, c) <- L.parens oldForExprs
      s <- statement
      return $ ForStmt a b c s
    oldForExprs = pure (,,) <*> (optional expression <* L.semi)
                            <*> (optional expression <* L.semi)
                            <*> optional expression
    newFor = do
      L.reserved "for"
      (a, b, c) <- L.parens newForExprs
      s <- statement
      return $ ForDeclStmt a b c s
    newForExprs = pure (,,) <*> declaration 
                            <*> (optional expression <* L.semi)
                            <*> optional expression
  
  jumpStmt :: Parser CStatement
  jumpStmt = choice [ goto, continue, break', return' ] where
    goto = GotoStmt <$> (L.reserved "goto" >> L.identifier)
    continue = L.reserved "continue" >> return ContinueStmt
    break' = L.reserved "break" >> return BreakStmt
    return' = ReturnStmt <$> (L.reserved "return" >> (optionMaybe expression <* L.semi))
    
