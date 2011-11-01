module Language.Pony.Transformations.Predefined.StringConcat where
  
  import Semantics.C
  import Data.Generics
  
  strlen l = FunctionCall (Ident "strlen") [Ident l]
  
  malloc l s = flip LStatement [] $ ExpressionS (Binary (Ident l) "=" (FunctionCall (Ident "alloca") [s]))
  
  strlcpy to from size = flip LStatement [] $ ExpressionS $ FunctionCall (Ident "strlcpy") [to, from, size]
  strlcat to from size = flip LStatement [] $ ExpressionS $ FunctionCall (Ident "strlcat") [to, from, size]
  
  checkForConcatenation :: Local -> [Local]
  checkForConcatenation (LStatement (ExpressionS (Binary (Ident a) "=" (Binary (Ident l) "<+>" (Ident r)))) li) = 
    [ LDeclaration var (li ++ [var])
    , malloc a (Ident "needed_size")
    , strlcpy (Ident a) (Ident l) (Ident "needed_size")
    , strlcat (Ident a) (Ident r) (Ident "needed_size")
    ]
      where 
        var = (Variable "needed_size" (STypedef "size_t" unsignedInt []) (Just (Binary (strlen l) "+" (strlen r))))
  checkForConcatenation x = [x]
  
  concatT :: GenericT
  concatT = mkT $ concatMap checkForConcatenation