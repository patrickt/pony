{-# LANGUAGE RankNTypes, TypeOperators, DeriveDataTypeable #-}

module Main where
  
  import Text.Parsec hiding (parseTest, (<|>))
  import Language.C
  import Language.C.Lexer as L
  import Semantics.C
  import Data.Generics
  import Data.Generics.Zipper
  import Language.Pony.Transformations
  import Language.Pony.Transformations.Utilities
  import Text.Pretty
  import System.IO.Unsafe
  
  f = "int main (int argc, char const *argv[]) { printf(\"Hello, world\"); return 0; }"
  prog = "struct __darwin_pthread_handler_rec { void (*__routine)(void *); void *__arg; struct __darwin_pthread_handler_rec *__next; };"
  
  ast = parseUnsafe preprocessedC f
  
  st = convert ast
  
  pr = pretty st
  