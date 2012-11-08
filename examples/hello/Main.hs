{-# LANGUAGE ViewPatterns #-}

module Main where
  
  import Language.Pony
  import Data.Functor.Fix
  import Language.C99
  import Semantics.C.Reifiable
  import Semantics.C.Pretty
  import System.IO
  import Text.PrettyPrint.Free
  
  changeHello :: Fix Sem -> Sem (Fix Sem)
  changeHello (out -> Name "hello") = FunCall (name' "printf") [str' "Hello from Pony"]
  changeHello otherwise = out otherwise
  
  main :: IO ()
  main = do 
    parsed <- preprocessAndParse preprocessedC "examples/hello/hello.pony.c" def
    case parsed of 
      (Left a) -> putStrLn "ERROR" >> print a
      (Right ast) -> do
        let asg = convert ast
        let transformed = ana changeHello asg
        let prettied = prettyPrint transformed
        handle <- openFile "./results.c" WriteMode
        hPutDoc handle prettied
        