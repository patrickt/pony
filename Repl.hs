{-# LANGUAGE RankNTypes, TypeOperators, DeriveDataTypeable #-}

module Main where
  
  import Text.Parsec hiding (parseTest, (<|>))
  import Language.C
  import Language.C.Lexer as L
  import Semantics.C
  import Data.Generics
  import Data.Generics.Zipper
  import Language.Pony.Transformation
  import System.IO.Unsafe
  
  prog = "typedef int rune_t;"
  
  ast = parseUnsafe preprocessedC prog
  
  st = convert ast
  
  pr = pretty st
  