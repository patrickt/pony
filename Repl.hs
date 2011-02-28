{-# LANGUAGE RankNTypes, TypeOperators, DeriveDataTypeable #-}

module Main where
  
  import Text.Parsec hiding (parseTest, (<|>))
  import Language.C
  import Language.C.Lexer as L
  import Semantics.C
  import Data.Generics
  import Data.Generics.Zipper
  import Language.Pony.SeparateDeclarations
  import System.IO.Unsafe
  
  prog = "int main() { int x = 5; dosomething(); int y = 10; somethingElse(); return 0;}"
  
  ast = parseUnsafe functionDefinition prog
  
  st = convertFunction ast
  
  pr = pretty st
  
  data Hello = MkHello deriving (Eq, Typeable, Data, Show)
  
  parseHello :: Parser Hello
  parseHello = pure MkHello <* L.symbol "hello"

  expression' :: Parser (Hello :+: CExpr)
  expression' = Inl <$> parseHello <|> Inr <$> expression