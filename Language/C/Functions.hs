module Language.C.Functions where
  
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
      (Named _ [Function args isVariadic]) -> do
        body <- compoundStmt
        return $ CFunction specs decl args body
      _ -> fail "something bad has happened"
  
  preprocessedC :: Parser [CExternal]
  preprocessedC = many1 extern where
    extern = (try (FunctionDecl <$> functionDefinition)) <|> (ExternDecl <$> declaration)
