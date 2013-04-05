module Language.C99.Statements 
  where
  
  import Language.C99.Parser
  import Language.C99.Expressions
  import Language.C99.Declarations
  import qualified Language.C99.Lexer as L
  import Language.C99.Syntax
  
  compound :: Parser CSyn
  compound = L.braces (group' <$> concat <$> (many (choice [ pure <$> statement, declarations])))
  
  -- TODO: move this
  opt :: Parser CSyn -> Parser CSyn
  opt p = fromMaybe <$> pure nil' <*> optional p
  
  statement :: Parser CSyn
  statement = choice
    [ labeledStmt
    , compoundStmt
    , jumpStmt
    , expressionStmt
    , selection
    , iteration
    ] <?> "C statement"
  
  compoundStmt :: Parser CSyn
  compoundStmt = L.braces (group' <$> concat <$> many blockItem) <?> "compound statement"
    where blockItem = try declarations <|> (pure <$> statement)
  
  -- TODO: dropping attributes on labels
  labeledStmt :: Parser CSyn
  labeledStmt = choice 
    [ case'          <$> (L.reserved "case" *> expression) <*> (L.colon *> statement) <?> "case statement"
    , default'       <$> (L.reserved "default" *> L.colon *> statement) <?> "default statement"
    , try  (labeled' <$> (identifier <* L.colon) <*> statement <?> "labeled statement")
    ]
  
  jumpStmt :: Parser CSyn
  jumpStmt = choice 
    [ goto'     <$> (L.reserved "goto" *> expression) <?> "goto statement"
    , continue' <$  L.reserved "continue" <?> "continue statement"
    , break'    <$  L.reserved "break" <?> "break statement"
    , return'   <$> (L.reserved "return" *> opt expression) <?> "return statement"
    ] <* L.semi
  
  expressionStmt :: Parser CSyn
  expressionStmt = (opt expression) <* L.semi
  
  selection :: Parser CSyn
  selection = ifStmt <|> switch where
    ifStmt = ifthenelse' <$> (L.reserved "if" *> L.parens expression)
                         <*> statement
                         <*> optional (L.reserved "else" *> statement) 
                         <?> "if statement"
    switch = switch' <$> (L.reserved "switch" *> L.parens expression) 
                     <*> statement
                     <?> "switch statement"
                     
  
  iteration :: Parser CSyn
  iteration = choice 
    [ while
    , doWhile
    , for 
    ] where
      while = while' <$> (L.reserved "while" *> parenExp) 
                     <*> statement 
                     <?> "while statement"
      doWhile = dowhile' <$> (L.reserved "do" *> statement) 
                         <*> (L.reserved "while" *> parenExp <* L.semi) 
                         <?> "do-while statement"
      -- TODO: declaration or expression at the beginning
      for = for' <$> (L.reserved "for" *> L.symbol "(" *> expressionStmt) 
                 <*> expressionStmt
                 <*> (expression <* L.symbol ")") 
                 <*> statement 
                 <?> "for statement"
      parenExp = L.parens expression
  