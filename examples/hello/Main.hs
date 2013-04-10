{-# LANGUAGE ViewPatterns #-}

module Main where
  
  import Language.Pony
  import System.IO
  
  changeHello :: CSyn -> C99 CSyn
  changeHello (out -> Name "hello") = Call (name' "printf") [cstr' "Hello from Pony"]
  changeHello otherwise = out otherwise
  
  main :: IO ()
  main = run $ def { anamorphisms = [changeHello] } 