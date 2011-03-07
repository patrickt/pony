module Language.Pony.Transformations.Predefined.HelloWorld where
  
  import Semantics.C
  import Data.Generics
  
  helloToPrintf :: Expression -> Expression
  helloToPrintf (Ident "hello") = FunctionCall (Ident "printf") [Ident "Hello from Pony!"]
  helloToPrintf x = x
  
  helloT :: GenericT
  helloT = mkT helloToPrintf