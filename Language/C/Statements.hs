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
            <|> selectionStmt
            <|> iterationStmt
            <|> expressionStmt
  
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
  selectionStmt = ifStmt <|> switch where
    ifStmt = do
      L.reserved "if"
      cond <- L.parens expression
      stmt <- statement
      elseBranch <- optionMaybe (L.reserved "if" >> statement)
      return $ IfStmt cond stmt elseBranch
    switch = do
      L.reserved "switch"
      expr <- L.parens expression
      stmt <- statement
      return $ SwitchStmt expr stmt
  
  -- This, my friends, is a hideous monstrosity. I am sorry.
  iterationStmt :: Parser CStatement
  iterationStmt = choice [ while, doWhile, oldFor, newFor ] where
    while = do 
      L.reserved "while"
      e <- L.parens expression
      s <- statement
      return $ WhileStmt e s
    doWhile = do 
      L.reserved "do"
      s <- statement
      L.reserved "while"
      e <- L.parens expression
      L.semi
      return $ DoWhileStmt s e
    oldFor = do
      L.reserved "for"
      (a, b, c) <- L.parens oldForExprs
      s <- statement
      return $ ForStmt a b c s
    oldForExprs = do
      a <- optionMaybe expression
      L.semi
      b <- optionMaybe expression
      L.semi
      c <- optionMaybe expression
      return (a, b, c)
    newFor = do
      L.reserved "for"
      (a, b, c) <- L.parens newForExprs
      s <- statement
      return $ ForDeclStmt a b c s
    newForExprs = do
      d <- declaration
      e <- optionMaybe expression
      L.semi
      f <- optionMaybe expression
      return (d, e, f)
  
  jumpStmt :: Parser CStatement
  jumpStmt = choice [ goto, continue, break', return' ] where
    goto = GotoStmt <$> (L.reserved "goto" >> L.identifier)
    continue = L.reserved "continue" >> return ContinueStmt
    break' = L.reserved "break" >> return BreakStmt
    return' = ReturnStmt <$> (L.reserved "return" >> optionMaybe expression)
    