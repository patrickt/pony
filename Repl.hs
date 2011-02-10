module Main where
  
  import Text.Parsec hiding (parseTest)
  import Language.C
  import Semantics.C
  import Data.Generics
  import Data.Generics.Zipper
  import Language.Pony.CheckMalloc
  import System.IO.Unsafe
  
  prog = "int main() { char *buffer = malloc(sizeof(char) * 4096); char* otherbuffer; otherbuffer = malloc(2049); return 0;}"
  
  ast = parseUnsafe functionDefinition prog
  
  st = convertFunction ast
  
  pr = pretty st