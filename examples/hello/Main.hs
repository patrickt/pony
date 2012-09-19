{-# LANGUAGE TemplateHaskell, EmptyDataDecls, TypeOperators, TypeFamilies, FlexibleContexts, UndecidableInstances #-}

module Main where
  
  import Generics.Regular.Rewriting
  import Language.Pony
  import Language.Pony.Rewriting

  helloRule :: Rule Expression
  helloRule = rule $ (Ident "hello") :~> FunctionCall (Ident "printf") [(CStr "Hello from Pony!")]

  helloToPrintf :: Expression -> Expression
  helloToPrintf x = rewrite helloRule x
  
  helloT :: GenericT
  helloT = mkT helloToPrintf
  
  main :: IO ()
  main = run $ pony {
    transformations = [MkTrans "Hello" TopDown helloT ]
  }