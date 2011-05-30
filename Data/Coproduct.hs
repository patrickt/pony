{-# LANGUAGE TypeOperators #-}

module Data.Coproduct 
  ( Co (..)
  , (:+:)
  )
  where
  
  
  data Co f = In (f (Co f))
  
  -- Swierstra's key insight was to combine expressions using the coproduct 
  -- of their signatures. Coproducts are easy to define, and are conceptually 
  -- similar to the Either union type; however, the coproduct operator (:+:) 
  -- operates on type constructors rather than types (as Either does).
  data (f :+: g) e = Inl (f e) | Inr (g e)
  infixr 6 :+:
  
  -- The coproduct of two functors is a functor.
  instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (Inl e1) = Inl (fmap f e1)
    fmap f (Inr e2) = Inr (fmap f e2)
  
  
  
  