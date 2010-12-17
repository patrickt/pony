module Language.C.TopLevel
  ( preprocessedC ) where
    
  import Language.C.Parser
  import Language.C.Lexer as L
  import Language.C.AST
  import Language.C.Declarations
  import Language.C.Functions
  
  preprocessedC :: Parser [CExternal]
  preprocessedC = (L.whiteSpace *> many extern) <* eof where
    extern = try (ExternDecl <$> declaration) <|> (FunctionDecl <$> functionDefinition)
  