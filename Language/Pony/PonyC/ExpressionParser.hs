-- This code will not work yet, since I haven't defined a class family for SemanticTree.

{-

module Language.Pony.PonyC.ExpressionParser where
  
  import Control.Applicative
  import Language.Pony.PonyC.Lexer
  import Text.Parsec.Expr
  
  expression = buildExpressionParser table statement <?> "C expression"
  
  statement =  labeledStatement
           <|> compoundStatement
           <|> expressionStatement
           <|> iterationStatement
           <|> jumpStatement
  
  -- 6.8.1
  labeledStatement =  (identifier >> colon >> statement)
                  <|> (symbol "case" >> constantExpression >> colon >> statement)
                  <|> (symbol "default" >> colon >> statement)
  
  -- 6.8.2
  -- This is wacky as all hell. Performance may suffer.
  compoundStatement = braces (optional blockItemList)
                      where
                        blockItemList =  blockItem <|> blockItemList >> blockItem
                        blockItem = declaration <|> statement
  
  -- 6.8.3
  expressionStatement = (optional expression) >> colon
  
  
  -- 6.8.4
  selectionStatement =  cIf >> parens expression >> statement
                    <|> cIf >> parens expression >> statement >> cElse >> statement
                    <|> switch >> parens expression >> statement
  
  -- 6.8.5
  iterationStatement =  while >> parens expression >> statement
                    <|> cDo >> statement >> while >> parens expression >> semi
                    <|> for >> parens forBody >> statement
                    <|> for >> parens forBody' >> statement 
                    where
                      while = reserved "while"
                      cDo = reserved "do"
                      optEx = optional expression
                      forBody = optEx >> semi >> optEx >> semi >> optEx
                      forBody' = declaration >> optEx >> semi >> optEx
  -- 6.8.6
  jumpStatement =  (reserved "goto") >> identifier >> colon
               <|> (reserved "continue") >> colon
               <|> (reserved "break") >> colon
               <|> (reserved "return") >> optional expression >> colon
  

-}