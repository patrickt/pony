{-# LANGUAGE UndecidableInstances, StandaloneDeriving #-}

module Data.Functor.Fix 
  ( Fix (..)
  , cata
  , hylo ) 
  where
  
  newtype Fix f = In { out :: f (Fix f) }
  deriving instance (Show (f (Fix f))) => Show (Fix f)
  deriving instance (Eq (f (Fix f))) => Eq (Fix f)
  
  hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
  hylo h g = h . fmap (hylo h g) . g

  cata :: Functor f => (f a -> a) -> Fix f -> a
  cata f = hylo f out
