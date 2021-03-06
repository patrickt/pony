{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances #-}

module Data.Coproduct 
  ( (:+:)
  , (:<:)
  )
  where
  
  inport Data.Functor.Fix
  
  -- Swierstra's key insight was to combine expressions using the coproduct 
  -- of their signatures. Coproducts are easy to define, and are conceptually 
  -- similar to the Either union type; however, the coproduct operator (:+:) 
  -- operates on type constructors rather than types (as Either does).
  data (f :+: g) e = Inl (f e) | Inr (g e)
  infixr 6 :+:
  
  -- Multiply-recursive coproducts.
  data (f :++: g) a b = Mnl (f a b) | Mnr (g a b)
  infixr 6 :++:
  
  -- The coproduct of two functors is a functor.
  instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (Inl e1) = Inl (fmap f e1)
    fmap f (Inr e2) = Inr (fmap f e2)
  
  -- This injection typeclass defines a mapping from one half of the coproduct
  -- (sub) to the the full coproduct (sup). See the instances below.
  -- XXX Do we need these to be Functor constrained? compdata does not; DALC does.
  class (Functor sub, Functor sup) => sub :<: sup where
    inj :: sub a -> sup a
    prj :: sup a -> Maybe (sub a) -- partial inverse of inj
  
  instance (Functor f) => f :<: f where
    inj = id
    prj = Just
  
  instance (Functor f, Functor g) => f :<: (f :+: g) where
    inj = Inl
    prj (Inl x) = Just x
    prj (Inr _) = Nothing
  
  -- This allows us to inject f into a larger h:+:g when f is already a part of g. Since g is on the right of the coproduct, so is f, but it requires slightly more hoops to jump through since f is sub g.
  instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
    inj = Inr . inj
    prj (Inr x) = prj x
    prj (Inl _) = Nothing
