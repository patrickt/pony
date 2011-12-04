module Main where
  
  import Language.Pony
  
  declare :: String -> SType -> Local
  declare n t = LDeclaration (Variable n t Nothing)
  
  partitionLocals :: [Local] -> ([Local], [Local])
  partitionLocals ls = partition ls ([], []) 
    where 
      partition [] them = them
      partition (d@(LDeclaration (Variable _ _ Nothing)) : rest) (a,b) = 
        partition rest (a ++ [d], b)
      partition (s@(LStatement st) : rest) (a, b) = 
        partition rest (a, b ++ [s])
      partition (d@(LDeclaration (Variable n t (Just e))) : rest) (a,b) =
        partition rest (a ++ [declare n t], 
                        b ++ [stmt $ Ident n .=. e])
  
  separate :: [Local] -> [Local]
  separate xs = a ++ b where (a, b) = partitionLocals xs
  
  separateT :: GenericT
  separateT = mkT separate
  
  main :: IO ()
  main = run $ pony {
    transformations = [MkTrans "SeparateDeclarations" TopDown separateT]
  }