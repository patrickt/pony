module Language.Pony.SeparateDeclarations where
  
  import Data.Generics
  import Language.C
  import Semantics.C
  import Semantics.C.Nodes
  
  partitionLocals :: [SLocal] -> ([SLocal], [SLocal])
  partitionLocals ls = partition ls ([], []) 
    where 
      partition [] them = them
      partition (d@(LDeclaration (Variable _ _ Nothing)) : rest) (a,b) = 
        partition rest (a ++ [d], b)
      partition (s@(LStatement st) : rest) (a, b) = 
        partition rest (a, b ++ [s])
      partition (d@(LDeclaration (Variable n t (Just e))) : rest) (a,b) =
        partition rest (a ++ [LDeclaration (Variable n t Nothing)], 
                        b ++ [LStatement (ExpressionS (Binary (Ident n) "=" e))])
  
  
  separate :: [SLocal] -> [SLocal]
  separate xs = a ++ b where (a, b) = partitionLocals xs
  
  separateT :: GenericT
  separateT = mkT separate