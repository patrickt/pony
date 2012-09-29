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
  
    -- Function :: Name -> Type -> Declarations -> [Anything localish] -> Function
    Function :: a -> a -> a -> [a] -> SFunction a
  
    -- Attributed :: Attribute -> Anything -> Anything
    Attributed :: [a] -> a -> Sem a
  
    -- types
    
    VoidT :: SType a
  
    -- Int :: Size -> Signedness -> Type
    IntT :: a -> a -> SType a
  
    FloatT :: SType a
    DoubleT :: SType a
    LongDoubleT :: SType a
  
    -- Char  :: Signedness -> Type
    CharT :: a -> SType a
  
    -- Pointer :: Type -> Type
    PointerToT :: a -> SType a
  
    -- Array :: Type -> Length -> Type
    ArrayT :: a -> a -> SType a
    
    -- Builtin :: Name -> Type
    BuiltinT :: a -> SType a
  
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
    CFloat   :: String -> SExpr a
    CChar    :: Char -> SExpr a
    Unary    :: a -> a -> SExpr a
    Binary   :: a -> a -> a -> SExpr a
    Ternary  :: a -> a -> a -> SExpr a
    Cast     :: a -> a -> SExpr a
    Brackets :: a -> a -> SExpr a
    FunCall  :: a -> [a] -> SExpr a
    VaArg    :: a -> a -> SExpr a
  
    -- attributes
    Auto     :: SAttr a
    Const    :: SAttr a
    Extern   :: SAttr a
    Inline   :: SAttr a
    Register :: SAttr a
    Restrict :: SAttr a
    Static   :: SAttr a
    Volatile :: SAttr a
    Custom   :: [a] -> SAttr a
  
    -- other stuff
    Program :: [a] -> Sem a
    
    -- gotta be a nicer way to do this
    -- name -> type -> initial value?
    Variable :: a -> a -> Maybe a -> Sem a
    Declarations :: [a] -> Sem a
    Typedef :: a -> a -> Sem a
  
  deriving instance (Show a) => Show (Sem a)
  deriving instance Functor Sem
  
  
  
  tie = In
  
  signed' t = In (t (In Signed))
  unsigned' t = In (t (In Unsigned))
  int' size sign = In (IntT (In (Size size)) (In sign))
  
  void' = In VoidT
  char' s = In (CharT s)
  name' n = In (Name n)
  
  