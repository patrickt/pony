module Language.C99.Declarations where
  
  import Language.C99.AST
  import Language.C99.Syntax
  import Language.C99.Parser
  
  declaration :: Parser CSyn
  
  sizedDeclaration :: Parser CField
  
  typeName :: Parser CTypeName
  
  declarator :: Parser CDeclarator