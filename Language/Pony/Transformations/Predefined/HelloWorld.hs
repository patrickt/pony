module Language.Pony.Transformations.Predefined.HelloWorld where
  
  import Language.C.Literals
  import Semantics.C
  import Data.Generics
  
  helloToPrintf :: Expression -> Expression
  helloToPrintf (Ident "hello") = FunctionCall (Ident "printf") [Literal (CString "Hello from Pony!")]
  helloToPrintf x = x
  
  helloT :: GenericT
  helloT = mkT helloToPrintf