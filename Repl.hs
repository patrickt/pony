module Main where
  
  import Text.Parsec hiding (parseTest)
  import Language.C
  import Semantics.C
  import Data.Generics.Zipper
  import Language.Pony.CheckMalloc
  import System.IO.Unsafe
  
  prog = "int main(int argc, char const** argv){ char *buffer = malloc(sizeof(char) * 4096); printf(\"Successfully allocated memory\\n\"); return 0; }"
  
  ast = parseUnsafe functionDefinition prog
  
  st = convertFunction ast
  
  pr = pretty st