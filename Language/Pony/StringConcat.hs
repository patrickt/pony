module Language.Pony.StringConcat where
  
  import Semantics.C
  import Data.Generics
  
  strlen l = FunctionCall (Ident "strlen") [(Ident l)]
  
  malloc l s = LStatement $ ExpressionS (Binary (Ident l) "=" (FunctionCall (Ident "alloca") [s]))
  
  strlcpy to from size = LStatement $ ExpressionS $ FunctionCall (Ident "strlcpy") [to, from, size]
  strlcat to from size = LStatement $ ExpressionS $ FunctionCall (Ident "strlcat") [to, from, size]
  
  checkForConcatenation :: SLocal -> [SLocal]
  checkForConcatenation (LStatement (ExpressionS (Binary (Ident a) "=" (Binary (Ident l) "<+>" (Ident r))))) = 
    [ LDeclaration (Variable "needed_size" (Typedef "size_t" unsignedInt []) 
        (Just (Binary (strlen l) "+" (strlen r))))
    , malloc a (Ident "needed_size")
    , strlcpy (Ident a) (Ident l) (Ident "needed_size")
    , strlcat (Ident a) (Ident r) (Ident "needed_size")
    ]
  checkForConcatenation x = [x]
  
  concatT :: GenericT
  concatT = mkT $ concatMap checkForConcatenation