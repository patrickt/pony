module Language.C99.TopLevel
  ( preprocessedC ) where
    
  import Language.C99.Parser
  import Language.C99.Lexer as L
  import Language.C99.AST
  import Language.C99.Declarations
  import Language.C99.Functions
  import Language.C99.Syntax
  
  -- | A parser action that parses a translation unit. It will fail if it
  -- finds any C-style comments, as the preprocessor removes them.
  preprocessedC :: Parser CSyn
  preprocessedC = program' <$> (L.whiteSpace *> many extern) <* eof where
    extern = choice 
      [ try declaration
      , functionDefinition
      ]
  