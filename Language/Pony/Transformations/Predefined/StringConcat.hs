module Language.Pony.Transformations.Predefined.StringConcat where
  
  import Semantics.C
  import Data.Generics
  
  strlen l = FunctionCall (Ident "strlen" []) [Ident l []] []
  
  malloc l s = flip LStatement [] $ ExpressionS (Binary (Ident l []) "=" (FunctionCall (Ident "alloca" []) [s] []) []) []
  
  strlcpy to from size = flip LStatement [] $ flip ExpressionS [] $ FunctionCall (Ident "strlcpy" []) [to, from, size] []
  strlcat to from size = flip LStatement [] $ flip ExpressionS [] $ FunctionCall (Ident "strlcat" []) [to, from, size] []
  
  checkForConcatenation :: Local -> [Local]
  checkForConcatenation (LStatement (ExpressionS (Binary (Ident a _) "=" (Binary (Ident l _) "<+>" (Ident r _) _) _) _) li) = 
    [ LDeclaration var (li ++ [name])
    , malloc a (Ident name [])
    , strlcpy (Ident a []) (Ident l []) (Ident name [])
    , strlcat (Ident a []) (Ident r []) (Ident name [])
    ]
      where 
        var = (Variable "needed_size" (STypedef "size_t" unsignedInt []) (Just (Binary (strlen l) "+" (strlen r) [])))
        name = "needed_size"
  checkForConcatenation x = [x]
  
  concatT :: GenericT
  concatT = mkT $ concatMap checkForConcatenation