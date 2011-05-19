module Language.C.Statements 
  ( statement, compoundStmt, asmStmt )
  where
  
  import Language.C.Parser
  import Language.C.Expressions
  import Language.C.AST
  import Language.C.Declarations
  import Language.C.Specifiers
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
  
  -- | A C statement. (C99 6.8.*)
  statement :: Parser CStatement
  statement = choice [ try labeledStmt
                     , asmStmt
                     , compoundStmt 
                     , jumpStmt 
                     , expressionStmt
                     , selectionStmt
                     , iterationStmt
                     ] <?> "C statement"
  
  labeledStmt :: Parser CStatement
  labeledStmt = choice [ caseStmt
                       , defaultStmt
                       , labelStmt
                       ] <?> "labeled statement" where
    caseStmt    = pure CaseStmt    <*> (L.reserved "case" *> expression) <*> (L.colon *> statement)
    defaultStmt = pure DefaultStmt <*> (L.reserved "default" *> L.colon *> statement)
    labelStmt   = pure LabeledStmt <*> (L.identifier <* L.colon) <*> many attribute <*> statement 
  
  -- | A compound statement, enclosed by braces. (C99 6.8.2)
  compoundStmt :: Parser CStatement
  compoundStmt = L.braces (pure CompoundStmt <*> many blockItem) <?> "compound statement"
  
  blockItem :: Parser BlockItem
  blockItem  =  try (pure Left <*> declaration) 
            <|> (pure Right <*> statement) 
            <?> "declaration or C statement"
            
  asmStmt :: Parser CStatement
  asmStmt =  pure AsmStmt 
         <*> (L.reserved "asm" *> optional volatile)
         <*> L.parens asmOperand where
           volatile = (L.reserved "volatile" *> pure QVolatile)
            
  asmOperand :: Parser AsmOperand
  asmOperand = try simple <|> complex where
    simple = pure Simple <*> (stringLiteral <* notFollowedBy L.colon)
    complex =  pure GCCAsm 
           <*> stringLiteral 
           <*> (L.colon *> optional stringLiteral)
           <*> (L.colon *> optional stringLiteral)
           <*> (L.colon *> optional stringLiteral)
  
  expressionStmt :: Parser CStatement
  expressionStmt = e <?> "expression" where 
    -- Can't figure out how to express this in applicative form
    e = do { expr <- optional expression; L.semi; return $ maybe EmptyStmt ExpressionStmt expr }
  
  selectionStmt :: Parser CStatement
  selectionStmt = ifStmt <|> switch where
    ifStmt = pure IfStmt <*> (L.reserved "if" *> L.parens expression)
                         <*> statement
                         <*> optional (L.reserved "else" *> statement) 
                         <?> "if statement"
    switch = pure SwitchStmt <*> (L.reserved "switch" *> L.parens expression) 
                             <*> statement 
                             <?> "switch statement"
  
  iterationStmt :: Parser CStatement
  iterationStmt = choice [ while
                         , doWhile
                         , try newFor
                         , oldFor 
                         ] where
    while   = pure WhileStmt <*> (L.reserved "while" *> L.parens expression) 
                             <*> statement 
                             <?> "while statement"
    doWhile = pure DoWhileStmt <*> (L.reserved "do" *> statement)
                               <*> (L.reserved "while" *> (L.parens expression <* L.semi))
                               <?> "do-while statement"
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
  jumpStmt = choice [ goto
                    , continue
                    , break'
                    , return' 
                    ] where
    goto     = pure GotoStmt     <*>  (L.reserved "goto" *> L.identifier)
    continue = pure ContinueStmt <*   L.reserved "continue"
    break'   = pure BreakStmt    <*   L.reserved "break"
    return'  = pure ReturnStmt   <*>  (L.reserved "return" *> (optional expression <* L.semi))
    
