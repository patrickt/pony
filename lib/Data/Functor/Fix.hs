module Data.Functor.Fix 
  ( module Data.Generics.Fixplate
  , Fix
  , tie
  , out
  , µ 
  , µ1
  , µ2
  , liftFix 
  ) 
  where
  
  import Data.Generics.Fixplate hiding (foldl, foldr, foldl1, foldr1, sequence, mapM, attribute)
  
  type Fix = Mu -- I don't know whether I like calling it Fix or Mu. We will find out.
  tie = Fix
  out = unFix
  µ = unFix
  
  µ1 :: Functor f => Fix f -> f (f (Fix f))
  µ1 = fmap µ . µ
  
  µ2 :: Functor f => Mu f -> f (f (f (Fix f)))
  µ2 = fmap µ1 . µ
  
  liftFix :: (Functor f) => (f (Fix f) -> a) -> Fix f -> a
  liftFix = (. unFix)
  