{-# LANGUAGE ViewPatterns #-}

module Main where
  
  import Language.Pony
  import Data.Functor.Fix
  import Language.C99
  import Semantics.C.Reifiable
  import Semantics.C.Pretty
  
  changeHello :: Fix Sem -> Sem (Fix Sem)
  changeHello (out -> Name "hello") = FunCall (name' "printf") [str' "Hello from Pony"]
  changeHello otherwise = out otherwise
  
  main :: IO ()
  main = do 
    f <- readFile "examples/hello/hello.pony.c"
    let ast = parseUnsafe preprocessedC f
    let asg = convert ast
    print asg
    let transformed = ana changeHello asg
    let done = prettyPrint transformed
    print done