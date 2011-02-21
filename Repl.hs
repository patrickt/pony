module Main where
  
  import Text.Parsec hiding (parseTest)
  import Language.C
  import Semantics.C
  import Data.Generics
  import Data.Generics.Zipper
  import Language.Pony.SeparateDeclarations
  import System.IO.Unsafe
  
  prog = "int main() { int x = 5; dosomething(); int y = 10; somethingElse(); return 0;}"
  
  ast = parseUnsafe functionDefinition prog
  
  st = convertFunction ast
  
  pr = pretty st