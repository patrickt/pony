{-# LANGUAGE ViewPatterns, OverloadedStrings #-}
module Main where
  
  import Language.Pony
  
  partitionStatements :: [CSyn] -> ([CSyn], [CSyn])
  partitionStatements xs = partition xs ([], [])
    where partition (v@(µ -> Variable _ _ Nothing) : rest) (a,b) =
            partition rest (a ++ [v], b)
          partition (v@(µ -> Variable t n (Just e)) : rest) (a,b) =
            partition rest (a ++ [variable' t n Nothing],
                            b ++ [binary' n "=" e])
          partition (other : rest) (a, b) =
            partition rest (a, b ++ [other])
          partition [] them = them
  
  separate :: CSyn -> C99 CSyn
  separate (µ -> Group xs) = Group $ a ++ b
    where (a, b) = partitionStatements xs
  separate other = out other
  
  main :: IO ()
  main = run $ def 
    { anamorphisms = [ separate ]
    }