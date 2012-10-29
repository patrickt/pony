{-# LANGUAGE DeriveDataTypeable, GADTs, StandaloneDeriving, DeriveFunctor, DeriveFoldable, DeriveTraversable, RankNTypes, OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Semantics.C.ASG where
  
  import Control.Applicative
  import Data.Fixed
  import Data.Functor.Fix
  import Data.Foldable (Foldable)
  import Data.Traversable (Traversable)
  import GHC.Exts (IsString (..))
  import Test.QuickCheck
  
  data Sem a where
    -- logical constructs
    Name     :: String -> Sem a -- used for binary operators as well as identifiers
    Enum     :: Sem a
    Signed   :: Sem a
    Size     :: Int -> Sem a
    Struct   :: Sem a
    Union    :: Sem a
    Unsigned :: Sem a
    Variadic :: Sem a
    
    -- modifiers for types
    ShortM     :: Sem a
    LongM      :: Sem a
    VeryLongM  :: Sem a
    
    Function :: { fname :: a, ftype :: a, fargs :: a, fbody :: a } -> Sem a 
    
    Arguments :: [a] -> Sem a
  
    -- Attributed :: Attribute -> Anything -> Anything
    Attributed :: [a] -> a -> Sem a
  
    -- types
    VoidT            :: Sem a
    IntT             :: { isign :: a, ibase :: a } -> Sem a
    FloatT           :: Sem a
    DoubleT          :: Sem a
    MultipartT       :: { mparts :: [a] } -> Sem a
    CharT            :: a -> Sem a -- Char  :: Signedness -> Type
    PointerToT       :: a -> Sem a -- Pointer :: Type -> Type
    ArrayT           :: { atype :: a, alength :: a } -> Sem a
    FunctionPointerT :: a -> a -> Sem a
    BuiltinT         :: a -> Sem a -- Builtin :: Name -> Type
    CompositeT       :: a -> Sem a
    TypedefT         :: a -> Sem a
    
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
    CFloat   :: Nano -> Sem a
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
    Prototype :: { pname :: a, ptype :: a, pargs :: a } -> Sem a
    CompositeInfo :: { ckind :: a, cname :: a, cfields :: a } -> Sem a
    Enumeration :: a -> [a] -> Sem a -- [Variables] -> Name? ->  Composite
    Program :: [a] -> Sem a
    Group :: [a] -> Sem a
    List  :: [a] -> Sem a
    
    -- gotta be a nicer way to do this
    -- type -> name -> initial value?
    Variable :: { vtype :: a, vname :: a, vvalue :: a } -> Sem a
    Typedef :: a -> a -> Sem a
    Sized :: a -> a -> Sem a
  
  deriving instance (Show a) => Show (Sem a)
  deriving instance (Eq a) => Eq (Sem a)
  deriving instance Functor Sem
  deriving instance Foldable Sem
  deriving instance Traversable Sem
  instance ShowF Sem where showsPrecF = showsPrec
  instance EqF Sem where equalF = (==)
  instance IsString (Sem a) where fromString = Name
  
  type CSem = forall a. Sem a
  type FSem = Fix Sem

  instance Arbitrary FSem where
    arbitrary = undefined



  isGroup :: CSem -> Bool
  isGroup (Group _) = True
  isGroup _ = False
  
  program = Fix . Program
  list = Fix . List
  nil = Fix Empty
  group = Fix . Group
  
  signed' t = Fix (t (Fix Signed))
  unsigned' t = Fix (t (Fix Unsigned))
  int' sign [] = Fix (IntT (Fix sign) nil)
  int' sign base = Fix (IntT (Fix sign) (Fix (MultipartT (tie <$> base))))
  variable a b c = tie $ Variable a b c
  fpointerto funcspecs params = tie $ FunctionPointerT funcspecs params
  
  void' :: Mu Sem
  void' = Fix VoidT
  char' s = Fix (CharT s)
  name' n = Fix (Name n)
  str' = Fix . CStr
  
  