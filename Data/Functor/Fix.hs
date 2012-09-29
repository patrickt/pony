{-# LANGUAGE UndecidableInstances, StandaloneDeriving #-}

module Data.Functor.Fix 
  ( Fix (..) ) 
  where
  
  newtype Fix f = In { out :: f (Fix f) }
  deriving instance (Show (f (Fix f))) => Show (Fix f)
  deriving instance (Eq (f (Fix f))) => Eq (Fix f)

