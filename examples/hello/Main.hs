module Main where
  
  import Language.Pony
  
  helloToPrintf :: Expression -> Expression
  helloToPrintf (Ident "hello") = FunctionCall (Ident "printf") [(CStr "Hello from Pony!")]
  helloToPrintf x = x
  
  helloT :: GenericT
  helloT = mkT helloToPrintf
  
  main :: IO ()
  main = run $ pony {
    transformations = [MkTrans "Hello" TopDown helloT ]
  }