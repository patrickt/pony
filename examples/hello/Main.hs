{-# LANGUAGE ViewPatterns, TemplateHaskell #-}

module Main where
  
  import Language.Pony
  import Language.C99.QuasiQuote
  
  changeHello :: CSyn -> Maybe CSyn
  changeHello (out -> Name "hello") = Just [expr|printf("Hello from Pony!");|] 
  changeHello _ = Nothing
  
  main :: IO ()
  main = runPony def (rewriteAll changeHello)