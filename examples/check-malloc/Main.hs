{-# LANGUAGE OverloadedStrings, ViewPatterns, NamedFieldPuns #-}

module Main where
  import Language.Pony
  
  checkMalloc :: CSyn -> C99 CSyn
  checkMalloc (µ -> Group stmts) = Group $ concatMap checkMalloc' stmts
  checkMalloc other = out other
  
  checkMalloc' :: CSyn -> [CSyn]
  checkMalloc' (µ -> v@(Variable { name, typ, value = Just (Fix (Call "malloc" _))})) = 
    [ Fix v, ifthenelse' (unary' "!" name) (call' "abort" []) Nothing ]
  checkMalloc' other = [other]
  
  main :: IO ()
  main = run $ def { anamorphisms = [checkMalloc] }
  