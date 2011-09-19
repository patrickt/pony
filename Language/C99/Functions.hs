module Language.C99.Functions where
  
  import Language.C99.Parser
  import Language.C99.AST
  import Language.C99.Specifiers
  import Language.C99.Declarations
  import Language.C99.Statements
  
  -- | C function definition (C99 6.9.1).
  functionDefinition :: Parser CFunction
  functionDefinition = do 
    oldState <- getState
    func <- CFunction <$> some specifier <*> declarator <*> compoundStmt
    putState oldState
    return func