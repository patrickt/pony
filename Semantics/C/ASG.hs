{-# LANGUAGE DeriveDataTypeable, GADTs, StandaloneDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Semantics.C.ASG where
  
  import Data.Foldable (Foldable)
  import Data.Functor.Fix
  import Data.Traversable (Traversable)

  
  data Sem a where
    -- logical constructs
    Name     :: String -> Sem a -- used for binary operators as well as identifiers
    Unsigned :: Sem a
    Signed   :: Sem a
    Size     :: Int -> Sem a
    Struct   :: Sem a
    Union    :: Sem a
    Ellipsis :: Sem a
  
    -- Function :: Name -> Type -> Arguments -> Group -> Function
    Function :: a -> a -> a -> a -> Sem a
    
    Arguments :: [a] -> Sem a
  
    -- Attributed :: Attribute -> Anything -> Anything
    Attributed :: [a] -> a -> Sem a
  
    -- types
    VoidT            :: Sem a
    IntT             :: a -> a -> Sem a -- Int :: Size -> Signedness -> Type
    FloatT           :: Sem a
    DoubleT          :: Sem a
    LongDoubleT      :: Sem a
    CharT            :: a -> Sem a -- Char  :: Signedness -> Type
    PointerToT       :: a -> Sem a -- Pointer :: Type -> Type
    ArrayT           :: a -> a -> Sem a -- Array :: Type -> Length -> Type
    FunctionPointerT :: a -> [a] -> Sem a
    BuiltinT         :: a -> Sem a -- Builtin :: Name -> Type
    CompositeT       :: a -> Sem a
    
    -- statements
    Break      :: Sem a
    Case       :: a -> [a] -> Sem a
    Continue   :: Sem a
    Compound   :: [a] -> Sem a
    Default    :: a -> Sem a
    DoWhile    :: a -> a -> Sem a
    Empty      :: Sem a
    For        :: a -> a -> a -> a -> Sem a
    Goto       :: a -> Sem a
    IfThen     :: a -> a -> Sem a
    IfThenElse :: a -> a -> a -> Sem a
    Labeled    :: a -> a -> Sem a
    Return     :: a -> Sem a
    Switch     :: a -> [a] -> Sem a
    While      :: a -> a -> Sem a
  
    -- expressions
    CStr     :: String -> Sem a
    CInt     :: Integer -> Sem a
    CFloat   :: String -> Sem a -- Maybe a fixed-point type?
    CChar    :: Char -> Sem a
    Unary    :: a -> a -> Sem a
    Binary   :: a -> a -> a -> Sem a
    Ternary  :: a -> a -> a -> Sem a
    Cast     :: a -> a -> Sem a
    Brackets :: a -> a -> Sem a
    FunCall  :: a -> [a] -> Sem a
    VaArg    :: a -> a -> Sem a
    Paren    :: a -> Sem a
  
    -- attributes
    Auto     :: Sem a
    Const    :: Sem a
    Extern   :: Sem a
    Inline   :: Sem a
    Register :: Sem a
    Restrict :: Sem a
    Static   :: Sem a
    Volatile :: Sem a
    Custom   :: [a] -> Sem a
  
    -- other stuff
    Composite :: a -> a -> [a] -> Sem a -- (Struct | Union) -> Name? -> Member
    Program :: [a] -> Sem a
    Group :: [a] -> Sem a
    List  :: [a] -> Sem a
    
    -- gotta be a nicer way to do this
    -- type -> name -> initial value?
    Variable :: a -> a -> a -> Sem a
    Typedef :: a -> a -> Sem a
    Sized :: a -> a -> Sem a
  
  deriving instance (Show a) => Show (Sem a)
  deriving instance (Eq a) => Eq (Sem a)
  deriving instance Functor Sem
  deriving instance Foldable Sem
  deriving instance Traversable Sem
  
  isFunction (Function _ _ _ _) = True
  isFunction _ = False
  
  program = In . Program
  list = In . List
  nil = In Empty
  tie = In
  
  signed' t = In (t (In Signed))
  unsigned' t = In (t (In Unsigned))
  int' size sign = In (IntT (In (Size size)) (In sign))
  variable a b c = tie $ Variable a b c
  fpointerto funcspecs params = tie $ FunctionPointerT funcspecs params
  
  void' :: Fix Sem
  void' = In VoidT
  char' s = In (CharT s)
  name' n = In (Name n)
  str' = In . CStr
  
  