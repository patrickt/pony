module Language.C.Functions where
  
  import Control.Monad
  import Language.C.Parser
  import Language.C.Lexer as L
  import Language.C.AST
  import Language.C.Specifiers
  import Language.C.Declarations
  import Language.C.Statements
  
  functionDefinition :: Parser CFunction
  functionDefinition = pure CFunction <*> some specifier
                                      <*> declarator
                                      <*> compoundStmt