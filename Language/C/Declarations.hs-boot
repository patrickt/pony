module Language.C.Declarations where
  
  import Language.C.AST
  import Language.C.Parser
  
  declaration :: Parser CDeclaration
  
  typeName :: Parser CDeclaration