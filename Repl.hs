module Main where
  
  import Text.Parsec hiding (parseTest)
  import Language.C.Parser
  import Language.C.AST
  import Language.C.Expressions
  import Language.C.Declarations
  import Language.C.Statements
  import Language.C.Specifiers
  import Language.C.Functions
  import Language.C.TopLevel
  import Semantics.C.Conversions
  import Semantics.C.Pretty
  import Semantics.C.Nodes
  import Data.Generics.Zipper
  import Language.Pony.CheckMalloc
  
  prog = "int main(int argc, char const** argv){ char *buffer = malloc(sizeof(char) * 4096); printf(\"Successfully allocated memory\\n\"); return 0; }"
  
  ast = parseUnsafe functionDefinition prog
  
  st = convertFunction ast
  
  pr = pretty st