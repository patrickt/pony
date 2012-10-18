{-# LANGUAGE OverloadedStrings, ViewPatterns, NamedFieldPuns, TypeSynonymInstances, FlexibleInstances #-}

module Main where
  
  import GHC.Exts
  import Language.Pony
  
  instance IsString (Fix Sem) where
    fromString = Fix . fromString
  
  checkMalloc :: Fix Sem -> Fix Sem
  checkMalloc (µ -> v@(Variable { vname, vtype, vvalue = (Fix (FunCall "malloc" args))})) = group [ tie (Ghost $ tie v) , tie $ (IfThen (Fix (Unary "!" vname)) (Fix (FunCall "abort" [nil]))) ]
  checkMalloc a = a
  
  main :: IO ()
  main = do 
    f <- readFile "examples/check-malloc/malloc.pony.c"
    let ast = parseUnsafe preprocessedC f
    let asg = convert ast
    print $ prettyPrint asg
    let transformed = transform checkMalloc asg
    print transformed
    let done = prettyPrint transformed
    print done
  