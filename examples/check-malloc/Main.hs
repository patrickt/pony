{-# LANGUAGE OverloadedStrings, ViewPatterns, NamedFieldPuns, TypeSynonymInstances, FlexibleInstances #-}

module Main where
  
  import GHC.Exts
  import Language.Pony
  import System.IO
  import Text.PrettyPrint.Free hiding (group)
  
  instance IsString (Fix Sem) where
    fromString = Fix . fromString
  
  checkMalloc :: Fix Sem -> Fix Sem
  checkMalloc (µ -> v@(Variable { vname, vtype, vvalue = (Fix (FunCall "malloc" args))})) = 
    group [ tie v , tie $ (IfThen (Fix (Unary "!" vname)) (Fix (FunCall "abort" [nil]))) ]
  checkMalloc a = tie $ fmap checkMalloc (out a)
  
  main :: IO ()
  main = do 
    parsed <- preprocessAndParse preprocessedC "examples/check-malloc/malloc.pony.c" def
    case parsed of 
      (Left a) -> putStrLn "ERROR" >> print a
      (Right ast) -> do
        let asg = convert ast
        let transformed = checkMalloc asg
        let prettied = prettyPrint transformed
        print prettied
