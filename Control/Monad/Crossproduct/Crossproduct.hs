{- This file describes a method for taking crossproducts over almost any monad. It is based off of the method described in "Composing Monads Using Coproducts", by Lüth and Ghani. -}

module Control.Monad.Crossproduct where
 
  {- First, we give an equivelant definition of a monad.-}
  class Functor t => Triple t where
    eta :: a -> t a
    mu  :: t (t a) -> t a
    etaInv :: t a -> Maybe a

  {- Now, we show that it's equivelant, and give us the ability to use monads. -}
    
  instance Triple f => Monad f where
    x >>= y = (mu . (fmap y)) x
    return = eta
             
  instance (Functor f, Monad f) => Triple f where
    eta = return
    mu x = x >>= id
    
  {- Now we define the cross product as the type Plus t1 t2, and give the functions necessary to construct such a monad. For more details, see Lüth, et al. -}
    
  data Plus t1 t2 a = T1 (t1 (Plus t1 t2 a))  
                    | T2 (t2 (Plus t1 t2 a))
                    | Var a
  foldProduct :: (Functor t1, Functor t2) => (a -> b) -> (t1 b -> b) -> (t2 b -> b) -> Plus t1 t2 a -> b
  foldProduct e f1 f2 (Var a) = e a
  foldProduct e f1 f2 (T1 t)  = f1 (fmap (foldProduct e f1 f2) t)
  foldProduct e f1 f2 (T2 t)  = f2 (fmap (foldProduct e f1 f2) t)
  
  strip1 :: Triple t1 => t1 (Plus t1 t2 a) -> Plus t1 t2 a
  strip1 t = case etaInv t of
               Just x  -> x
               Nothing -> T1 t
  
  strip2 :: Triple t2 => t2 (Plus t1 t2 a) -> Plus t1 t2 a
  strip2 t = case etaInv t of 
               Just x  -> x
               Nothing -> T2 t
               
  strip :: (Triple t1, Triple t2) => Plus t1 t2 a -> Plus t1 t2 a
  strip  = foldProduct Var strip1 strip2
  
  lift1 :: Triple t1 => Plus t1 t2 a -> t1 (Plus t1 t2 a)
  lift1 (T1 t) = t
  lift1 t      = eta t
  
  lift2 :: Triple t2 => Plus t1 t2 a -> t2 (Plus t1 t2 a)
  lift2 (T2 t) = t
  lift2 t      = eta t
  
  wit1 :: Triple t1 => t1 (Plus t1 t2 a) -> Plus t1 t2 a
  wit1 t = strip1 (mu (fmap lift1 t))
  
  wit2 :: Triple t2 => t2 (Plust t1 t2 a) -> Plus t1 t2 a
  wit2 t = strip2 (mu (fmap lift2 t))
  
  wit :: (Triple t1, Triple t2) => Plus t1 t2 a -> Plus t1 t2 a
  wit  = foldProduct Var wit1 wit2
  
  {- Now, we give some instances to Plus values, so that we can actually see that they ARE monads! -}
  
  instance (Functor t1, Functor t2) => Functor (Plus t1 t2) where
    fmap f (T1 t)  = T1 (fmap (fmap f) t)
    fmap f (T2 t)  = T2 (fmap (fmap f) t)
    fmap f (Var x) = Var (f x)
    
  instance (Triple t1, Triple t2) => Triple (Plus t1 t2) where
    eta x           = Var x
    etaInv (Var x ) = Just x
    etaInv (T1 t)   = Nothing
    etaInv (T2 t)   = Nothing
    mu              = foldProduct id wit1 wit2
    
  {- We want to give some injections from a monad into a crossproduct (think of it as the Categorical version of \/) -}
    
  inl :: Triple t1 => t1 a -> Plus t1 t2 a
  inl t = T1 (fmap Var t)
  
  inr :: Triple t2 => t2 a -> Plus t1 t2 a
  inr t = T2 (fmap Var t)
  
  
  {- Finally, we define taking a coproduct as a function. -}
  
  coprod :: (Triple t1, Triple t2, Triple s) =>
            (forall a. t1 a -> s a) ->
            (forall a. t2 a -> s a) -> Plus t1 t2 a -> s a
  coprod f g = foldProduct eta (mu . f) (mu . g)