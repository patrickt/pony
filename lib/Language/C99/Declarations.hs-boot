module Language.C99.Declarations where
  
  import Language.C99.AST
  import Language.C99.Syntax
  import Language.C99.Parser
  
  declarations :: Parser [CSyn]
  
  sizedDeclarations :: Parser [CSyn]
  
  typeName :: Parser CSyn
  
  declarator :: Parser CDeclarator