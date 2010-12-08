module Language.C.Functions where
  
  import Control.Monad
  import Language.C.Parser
  import Language.C.Lexer as L
  import Language.C.AST
  import Language.C.Specifiers
  import Language.C.Declarations
  import Language.C.Statements
  
  functionDefinition :: Parser CFunction
  functionDefinition = do
    specs <- many specifier
    decl <- declarator
    case decl of
      (Named _ [Function args isVariadic] _) -> do
        body <- compoundStmt
        return $ CFunction specs decl args body
      _ -> mzero
  
  preprocessedC :: Parser [CExternal]
  preprocessedC = ((L.whiteSpace *> many extern) <* eof) where
    extern  = try (ExternDecl <$> declaration) 
           <|> (FunctionDecl <$> functionDefinition)
