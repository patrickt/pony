{-# LANGUAGE OverloadedStrings #-}

module Main where
  
  import Language.Pony
  
  strlen l = FunctionCall (Ident "strlen") [Ident l]
  
  malloc l s = stmt $ ((Ident l) .=. (FunctionCall "alloca") [s])
  
  strlcpy to from size = stmt $ FunctionCall (Ident "strlcpy") [to, from, size]
  strlcat to from size = stmt $ FunctionCall (Ident "strlcat") [to, from, size]
  
  checkForConcatenation :: Local -> [Local]
  checkForConcatenation (LStatement (ExpressionS (Binary (Ident a) "=" (Binary (Ident l) "<+>" (Ident r))))) = 
    [ LDeclaration (Variable "needed_size" (STypedef "size_t" unsignedInt []) 
        (Just (Binary (strlen l) "+" (strlen r))))
    , malloc a (Ident "needed_size")
    , strlcpy (Ident a) (Ident l) (Ident "needed_size")
    , strlcat (Ident a) (Ident r) (Ident "needed_size")
    ]
  checkForConcatenation x = [x]
  
  concatT :: GenericT
  concatT = mkT $ concatMap checkForConcatenation
  
  main :: IO ()
  main = run $ pony {
    operators = [Arithmetic "<+>"],
    transformations = [ MkTrans "StringConcat" TopDown concatT ]
  }