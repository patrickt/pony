module Data.Coproduct 
  ( module Data.Comp
  , module Data.Comp.Show
  , module Data.Comp.Equality
  , module Data.Comp.Derive
  , µ
  , µ1
  , µf
  , π
  ) where
    
  import Data.Comp
  import Data.Comp.Show
  import Data.Comp.Equality
  import Data.Comp.Derive
  
  µ :: (Functor f) => Term f -> f (Term f)
  µ = unTerm
  
  µ1 :: (Functor f) => Term f -> f (f (Term f))
  µ1 = fmap µ . µ
  
  µ2 :: (Functor f) => Term f -> f (f (f (Term f)))
  µ2 = fmap µ1 . µ
  
  π :: (Functor f) => f (a, b) -> f b
  π = fmap snd
  
  µf :: (Functor f) => f (Term f, b) -> f (f (Term f))
  µf = fmap (µ . fst)