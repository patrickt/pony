module Language.Pony.Transformations.Predefined.HelloWorld where
  
  import Language.C99.Literals
  import Semantics.C
  import Data.Generics
  
  helloToPrintf :: Expression -> Expression
  helloToPrintf (Ident "hello" a) = FunctionCall (Ident "printf" a) [(CStr "Hello from Pony!" a)] a
  helloToPrintf x = x
  
  helloT :: GenericT
  helloT = mkT helloToPrintf