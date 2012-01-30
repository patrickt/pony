module Language.Pony.Transformations.Predefined.SeparateDeclarations where
  
  import Data.Generics
  import Language.C99
  import Semantics.C
  
  declare :: String -> SType -> Local
  declare n t = LDeclaration (Variable n t Nothing) []
  
  (.=.) :: Expression -> Expression -> Local
  a .=. b = flip LStatement [] $ ExpressionS (Binary a "=" b []) []
  
  partitionLocals :: [Local] -> ([Local], [Local])
  partitionLocals ls = partition ls ([], []) 
    where 
      partition [] them = them
      partition (d@(LDeclaration (Variable _ _ Nothing) _) : rest) (a,b) = 
        partition rest (a ++ [d], b)
      partition (s@(LStatement st _) : rest) (a, b) = 
        partition rest (a, b ++ [s])
      partition (d@(LDeclaration (Variable n t (Just e)) _) : rest) (a,b) =
        partition rest (a ++ [declare n t], 
                        b ++ [Ident n [] .=. e ])
  
  separate :: [Local] -> [Local]
  separate xs = a ++ b where (a, b) = partitionLocals xs
  
  separateT :: GenericT
  separateT = mkT separate