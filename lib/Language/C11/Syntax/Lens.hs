{-# LANGUAGE KindSignatures #-}

module Language.C11.Syntax.Lens 
  ( HasType (..)
  , HasName (..)
  , CanName (..)
  , HasArguments (..)
  , HasBody (..)
  , HasTarget (..)
  , HasValue (..)
  , module Language.C11.Syntax.Names
  ) where
  
  import Language.Pony.Overture
  import Control.Lens hiding (Context, Const)
  import Data.Coproduct
  import StringTable.Atom
  import Language.C11.Syntax.Names
  
  class HasType f where
    typ :: Lens' (f a) a
   
  makeClassy ''Name
  
  class CanName f where
    nameT :: Traversal' (f a) ByteString
    
  
  sumPrism :: (g :<: f) => Prism' (f a) (g a)
  sumPrism = prism' inj proj
  
  sumPrism' :: (Applicative f, Choice p, g :<: f1, g1 :<: f1) =>
     p (g1 (Cxt h f1 a)) (f (g (Cxt h f1 a)))
     -> p (Cxt h f1 a) (f (Cxt h f1 a))
  sumPrism' = prism' inject project
  
  type FTerm = Cxt NoHole
  

  sumPrismC :: (Functor g, Functor g1, g :<: f1, g1 :<: f1) => Prism (Cxt h f1 a) (Cxt h f1 a) (Const g1) (Const g)
  sumPrismC = prism' injectConst projectConst
  
  sumPrism2 :: (Functor g, g :<: f1, g1 :<: f1) => Prism (Context f1 a) (Context f1 a) (g1 (Context f1 a)) (Context g (Context f1 a))
  sumPrism2 = prism' injectCxt project

  -- -- The real type of this is 'Prism (PTerm f1) (PTerm f1) (g (PTerm f1)) (g (PTerm f1))
  -- sumPrism':: (g :<: f1, g1 :<: f1) => Prism (Cxt h f1 a) (Cxt h f1 a) (g1 (Cxt h f1 a)) (g (Cxt h f1 a))
  -- sumPrism' = prism' inject project
  
  
  
  -- toEither :: (f :+: g) a -> Either (g a) (f a)
  -- toEither = caseF Left Right
  
  -- liftSum ''CanName
  
  class HasArguments f where
    arguments :: Lens' (f a) [a]
  
  class HasBody f where
    body :: Lens' (f a) [a]
  
  class HasTarget f where
    target :: Lens' (f a) a
    
  class HasValue f v | f -> v where
    value :: Lens' (f a) v
  