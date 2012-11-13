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
  main = run $ def { anamorphisms = [changeHello] } 