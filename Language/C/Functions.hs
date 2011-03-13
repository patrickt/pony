module Language.C.Functions where
  
  import Language.C.Parser
  import Language.C.AST
  import Language.C.Specifiers
  import Language.C.Declarations
  import Language.C.Statements
  
  -- | C function definition (C99 6.9.1).
  functionDefinition :: Parser CFunction
  functionDefinition = pure CFunction <*> some specifier
                                      <*> declarator
                                      <*> compoundStmt