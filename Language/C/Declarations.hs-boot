module Language.C.Declarations where
  
  import Language.C.AST
  import Language.C.Parser
  
  declaration :: Parser CDeclaration
  
  sizedDeclaration :: Parser CField
  
  typeName :: Parser CTypeName
  
  declarator :: Parser CDeclarator