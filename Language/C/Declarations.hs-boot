module Language.C.Declarations where
  
  import Language.C.AST
  import Language.C.Parser
  
  declaration :: Parser CDeclaration
  
  sizedDeclaration :: Parser CDeclaration
  
  typeName :: Parser CDeclaration
  
  declarator :: Parser CDeclarator