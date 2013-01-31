{-# LANGUAGE ViewPatterns #-}

module Main where
  
  import Language.Pony
  import System.IO
  
  changeHello :: Fix Sem -> Sem (Fix Sem)
  changeHello (out -> Name "hello") = FunCall (name' "printf") [str' "Hello from Pony"]
  changeHello otherwise = out otherwise
  
  main :: IO ()
  main = run $ def { anamorphisms = [changeHello] } 