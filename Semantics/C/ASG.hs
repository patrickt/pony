{-# LANGUAGE DeriveDataTypeable, GADTs, KindSignatures, StandaloneDeriving, DeriveFunctor #-}

module Semantics.C.ASG where
  
  import Data.Monoid
  import Language.Pony.Prelude
  import Data.Generics
  import Language.Pony.MachineSizes
  
  catamorph :: Functor f => (f a -> a) -> Fix f -> a
  catamorph f (In t) = f (fmap (catamorph f) t)

  newtype Fix f = In { out :: f (Fix f) }
  
  type SName       = Sem
  type SFunction   = Sem
  type SType       = Sem
  type SParam      = Sem
  type SAttr       = Sem
  type SLocal      = Sem
  type SSignedness = Sem
  type SSize       = Sem
  type SStatement  = Sem
  type SExpr       = Sem
  
  data Sem :: (* -> *) where
    -- logical constructs
    Name     :: String -> SName a -- used for binary operators as well as identifiers
    Unsigned :: SSignedness a
    Signed   :: SSignedness a
    Size     :: Int -> SSize a
  
    -- Function :: Name -> Type -> [Param] -> [Local] -> Function
    Function :: a -> a -> [a] -> [a] -> SFunction a
  
    -- Attributed :: Attribute -> Anything -> Anything
    Attributed :: [a] -> a -> Sem a
  
    -- types
  
    -- it's void
    Void :: SType a
  
    -- Int :: Size -> Signedness -> Type
    IntT :: a -> a -> SType a
  
    -- Float :: Size -> Type
    FloatT :: a -> SType a
  
    -- Char  :: Signedness -> Type
    CharT :: a -> SType a
  
    -- Pointer :: Type -> Type
    PointerTo :: a -> SType a
  
    -- Array :: Type -> Length -> Type
    Array :: a -> a -> SType a
  
    -- TODO: composite types
  
    -- statements
    Break      :: SStatement a
    Case       :: a -> [a] -> SStatement a
    Continue   :: SStatement a
    Compound   :: [a] -> SStatement a
    Default    :: a -> SStatement a
    DoWhile    :: a -> a -> SStatement a
    Empty      :: SStatement a
    For        :: Maybe a -> Maybe a -> Maybe a -> a -> SStatement a
    Goto       :: a -> SStatement a
    IfThen     :: a -> a -> SStatement a
    IfThenElse :: a -> a -> a -> SStatement a
    Labeled    :: a -> a -> SStatement a
    Return     :: Maybe a -> SStatement a
    Switch     :: a -> [a] -> SStatement a
    While      :: a -> a -> SStatement a
  
    -- expressions
    CStr     :: String -> SExpr a
    CInt     :: Integer -> SExpr a
    CFloat   :: Double -> SExpr a
    Unary    :: a -> a -> SExpr a
    Binary   :: a -> a -> a -> SExpr a
    Ternary  :: a -> a -> a -> SExpr a
    Cast     :: a -> a -> SExpr a
    Brackets :: a -> a -> SExpr a
    FunCall  :: a -> [a] -> SExpr a
  
    -- attributes
    Auto     :: SAttr a
    Const    :: SAttr a
    Extern   :: SAttr a
    Inline   :: SAttr a
    Register :: SAttr a
    Restrict :: SAttr a
    Static   :: SAttr a
    Volatile :: SAttr a
    Custom   :: a -> SAttr a
  
    -- other stuff
    Variable :: a -> a -> Maybe a -> Sem a
  
  deriving instance (Show a) => Show (Sem a)
  deriving instance Functor Sem
  
  signed' t = In (t (In Signed))
  unsigned' t = In (t (In Unsigned))
  int' size sign = In (IntT (In (Size size)) (In sign))
  
  void' = In Void
  char' s = In (CharT s)
  name' n = In (Name n)
  
  