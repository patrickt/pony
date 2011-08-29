module Language.C99.Statements 
  ( statement, compoundStmt, asmStmt )
  where
  
  import Language.C99.Parser
  import Language.C99.Expressions
  import Language.C99.AST
  import Language.C99.Declarations
  import Language.C99.Specifiers
  import qualified Language.C99.Lexer as L
  
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
    caseStmt    = CaseStmt    <$> (L.reserved "case" *> expression) <*> (L.colon *> statement)
    defaultStmt = DefaultStmt <$> (L.reserved "default" *> L.colon *> statement)
    labelStmt   = LabeledStmt <$> (L.identifier <* L.colon) <*> many attribute <*> statement 
  
  -- | A compound statement, enclosed by braces. (C99 6.8.2)
  compoundStmt :: Parser CStatement
  compoundStmt = L.braces (CompoundStmt <$> many blockItem) <?> "compound statement"
  
  blockItem :: Parser CBlockItem
  blockItem  =  try (Left <$> declaration) 
            <|> (Right <$> statement) 
            <?> "declaration or C statement"
            
  asmStmt :: Parser CStatement
  asmStmt =  AsmStmt 
         <$> (L.reserved "asm" *> optional volatile)
         <*> L.parens asmOperand where
           volatile = L.reserved "volatile" *> pure CVolatile
            
  asmOperand :: Parser CAsmOperand
  asmOperand = try simple <|> complex where
    simple = Simple <$> (stringLiteral <* notFollowedBy L.colon)
    complex =  GCCAsm 
           <$> stringLiteral 
           <*> (L.colon *> L.commaSep asmArgument)
           <*> (L.colon *> L.commaSep asmArgument)
           <*> (L.colon *> L.commaSep stringLiteral)

  asmArgument :: Parser CAsmArgument
  asmArgument = CAsmArgument <$> stringLiteral <*> (optional $ L.parens identifier)
  
  expressionStmt :: Parser CStatement
  expressionStmt = maybe EmptyStmt ExpressionStmt <$> optional expression <* L.semi
    
  selectionStmt :: Parser CStatement
  selectionStmt = ifStmt <|> switch where
    ifStmt = IfStmt <$> (L.reserved "if" *> L.parens expression)
                    <*> statement
                    <*> optional (L.reserved "else" *> statement) 
                    <?> "if statement"
    switch = SwitchStmt <$> (L.reserved "switch" *> L.parens expression) 
                        <*> statement 
                        <?> "switch statement"
  
  iterationStmt :: Parser CStatement
  iterationStmt = choice [ while
                         , doWhile
                         , try newFor
                         , oldFor 
                         ] where
    while   = WhileStmt <$> (L.reserved "while" *> L.parens expression) 
                        <*> statement 
                        <?> "while statement"
    doWhile = DoWhileStmt <$> (L.reserved "do" *> statement)
                          <*> (L.reserved "while" *> (L.parens expression <* L.semi))
                          <?> "do-while statement"
    oldFor = do
      L.reserved "for"
      (a, b, c) <- L.parens oldForExprs
      s <- statement
      return $ ForStmt a b c s
    oldForExprs = (,,) <$> (optional expression <* L.semi)
                       <*> (optional expression <* L.semi)
                       <*> optional expression
    newFor = do
      L.reserved "for"
      (a, b, c) <- L.parens newForExprs
      s <- statement
      return $ ForDeclStmt a b c s
    newForExprs = (,,) <$> declaration 
                       <*> (optional expression <* L.semi)
                       <*> optional expression
  
  jumpStmt :: Parser CStatement
  jumpStmt = choice [ goto
                    , continue
                    , break'
                    , return' 
                    ] where
    goto     = GotoStmt          <$>  (L.reserved "goto" *> expression)
    continue = pure ContinueStmt <*   L.reserved "continue"
    break'   = pure BreakStmt    <*   L.reserved "break"
    return'  = ReturnStmt        <$>  (L.reserved "return" *> (optional expression <* L.semi))
    
