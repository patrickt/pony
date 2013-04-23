{-# LANGUAGE ViewPatterns, OverloadedStrings #-}
module Main where
  
  import Language.Pony
  
  partitionMap :: (a -> (Maybe b, Maybe c)) -> [a] -> ([b],[c])
  partitionMap f = foldr go ([], [])
    where go a (bs, cs) = let (b, c) = f a
                          in (maybe bs (:bs) b, maybe cs (:cs) c)
  
  partitionStatements :: [CSyn] -> ([CSyn], [CSyn])
  partitionStatements = partitionMap go
    where go v@(µ -> Variable _ _ Nothing)  = (Just v, Nothing)
          go v@(µ -> Variable t n (Just e)) = (Just $ variable' t n Nothing,
                                               Just $ binary' n "=" e)
          go other                          = (Nothing, Just other)
    
  separate :: CSyn -> C99 CSyn
  separate (µ -> Group xs) = Group $ a ++ b
    where (a, b) = partitionStatements' xs
  separate other = out other
  
  main :: IO ()
  main = run $ def 
    { anamorphisms = [ separate ]
    }