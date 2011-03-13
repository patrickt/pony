module Language.C.TopLevel
  ( preprocessedC ) where
    
  import Language.C.Parser
  import Language.C.Lexer as L
  import Language.C.AST
  import Language.C.Declarations
  import Language.C.Functions
  
  -- | A parser action that parses a translation unit. It will fail if it
  -- finds any C-style comments, as the preprocessor removes them.
  preprocessedC :: Parser CTranslationUnit
  preprocessedC = (L.whiteSpace *> many extern) <* eof where
    extern = try (ExternDecl <$> declaration) <|> (FunctionDecl <$> functionDefinition)
  