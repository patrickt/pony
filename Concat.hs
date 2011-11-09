{-# LANGUAGE OverloadedStrings #-}

module Concat where
  
  import Language.Pony
  
  strlen l = FunctionCall (Ident "strlen") [Ident l]
  
  malloc l s = stmt $ ((Ident l) .=. (FunctionCall "alloca") [s])
  
  strlcpy to from size = stmt $ FunctionCall (Ident "strncpy") [to, from, size]
  strlcat to from size = stmt $ FunctionCall (Ident "strncat") [to, from, size]
  
  checkForConcatenation :: Local -> [Local]
  checkForConcatenation loc@(LStatement (ExpressionS (Binary (Ident a) "=" (Binary (Ident l) "<+>" (Ident r)))) b) =  let 
    needed = getFreeVariable "needed_size" loc
     in
     [ LDeclaration (Variable needed (STypedef "unsigned int" unsignedInt []) (Just (Binary (strlen l) "+" (strlen r)))) (b ++ [needed])
     , malloc a (Ident needed)
     , strlcpy (Ident a) (Ident l) (Ident needed)
     , strlcat (Ident a) (Ident r) (Ident needed)
     ]
  checkForConcatenation x = [x]
      

  concatT :: GenericT
  concatT = mkT $ concatMap checkForConcatenation
  
  main :: IO ()
  main = run $ pony {
    operators = [Arithmetic "<+>"],
    transformations = [ MkTrans "StringConcat" TopDown concatT ]
  }