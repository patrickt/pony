{-# LANGUAGE UndecidableInstances, StandaloneDeriving #-}

module Data.Functor.Fix 
  ( Fix (..)
  , cata
  , hylo
  , para ) 
  where
    
  import Control.Category ((<<<))
  
  -- fixed-point type
  newtype Fix f = In { out :: f (Fix f) }
  -- needs UndecidableInstances for this part
  deriving instance (Show (f (Fix f))) => Show (Fix f)
  deriving instance (Eq (f (Fix f))) => Eq (Fix f)

  -- a hylomorphism is a the combination of an unfold and a fold
  hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
  hylo fold unfold thing = (fold <<< fmap (hylo fold unfold) <<< unfold) thing

  -- a catamorphism is just a fold over the fixed point of a functor
  cata :: Functor f => (f b -> b) -> Fix f -> b
  cata f = hylo f out

  -- a paramorphism is a catamorphism combined with a copy of the input, useful for pattern matching
  para :: Functor f => (Fix f -> f b -> b) -> Fix f -> b
  para h = go where go t = h t (fmap go $ out t)

