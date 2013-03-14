{-# LANGUAGE UndecidableInstances #-}

module Language.C99.Syntax where
  
  import Control.Applicative hiding (Const)
  import Data.Fixed
  import Data.Functor.Fix
  import GHC.Exts (IsString (..))
  import Test.QuickCheck
  
  type CSyn = Mu C99
  
  data C99 a where
    -- logical constructs
    Name     :: { getName :: String } -> C99 a -- used for binary operators as well as identifiers
    
    Signed   :: a -> C99 a
    Unsigned :: a -> C99 a
    ShortM   :: a -> C99 a
    LongM    :: a -> C99 a
    Attributed :: [a] -> a -> C99 a
  
    -- types
    VoidT            :: C99 a
    IntT             :: C99 a
    FloatT           :: C99 a
    DoubleT          :: C99 a
    CharT            :: C99 a
    VeryLongT        :: C99 a
    Struct           :: C99 a
    Union            :: C99 a
    PointerToT       :: a -> C99 a -- Pointer :: Type -> Type
    ArrayT           :: { len :: a, typ :: a } -> C99 a
    FunctionPointerT :: a -> a -> C99 a
    BuiltinT         :: a -> C99 a -- Builtin :: Name -> Type
    TypedefT         :: a -> C99 a
    BoolT            :: C99 a
    TypeOfT          :: a -> C99 a
    
    Function :: { ftype :: a, fname :: a, fargs :: a, fbody :: a } -> C99 a
    
    -- statements
    Break      :: C99 a
    Case       :: a -> a -> C99 a
    Continue   :: C99 a
    Default    :: a -> C99 a
    DoWhile    :: a -> a -> C99 a
    Empty      :: C99 a
    For        :: a -> a -> a -> a -> C99 a
    Goto       :: a -> C99 a
    IfThenElse :: a -> a -> a -> C99 a
    Labeled    :: a -> a -> C99 a
    Return     :: a -> C99 a
    Switch     :: a -> a -> C99 a -- Buggy: should have a Group as its second parameter
    While      :: a -> a -> C99 a
  
    -- expressions
    CStr     :: { getString :: String } -> C99 a 
    CInt     :: Integer -> C99 a
    CFloat   :: String -> C99 a
    CChar    :: Char -> C99 a
    CommaSep :: a -> a -> C99 a
    Unary    :: a -> a -> C99 a
    Binary   :: a -> a -> a -> C99 a
    Ternary  :: a -> a -> a -> C99 a
    Cast     :: [a] -> a -> C99 a
    Index    :: a -> a -> C99 a -- rename to Index?
    Call     :: a -> [a] -> C99 a -- rename to Call?
    Access   :: a -> a -> a -> C99 a
    VaArg    :: a -> a -> C99 a
    Paren    :: a -> C99 a
  
    -- attributes
    Auto     :: a -> C99 a
    Const    :: a -> C99 a
    Extern   :: a -> C99 a
    Inline   :: a -> C99 a
    Register :: a -> C99 a
    Restrict :: a -> C99 a
    Static   :: a -> C99 a
    Volatile :: a -> C99 a
    
    Initializer :: [a] -> C99 a
    
    -- other stuff
    Enumeration :: { name :: a, members :: a } -> C99 a
    Composite :: { kind :: a, name :: a, fields :: a } -> C99 a
    Program :: [a] -> C99 a
    Group :: [a] -> C99 a
    List  :: [a] -> C99 a -- do we use this anywhere?
    Assembly :: { isVolatile :: Bool, text :: a, inRegs :: a, outRegs :: a, clobberList :: a } -> C99 a
    AssemblyOperand :: { opconstraint :: a, opvar :: a } -> C99 a
    
    Arguments :: [a] -> Bool -> C99 a
    ForwardTypeDeclaration :: a -> C99 a
    Variable :: { typ :: a, name :: a, value :: a } -> C99 a
    MultiDeclaration :: { components :: [a] } -> C99 a
    Typedef :: { typ :: a, name :: a } -> C99 a
    Sized :: a -> a -> C99 a
    
  data Foo = Bar { baz :: Int} | Baf { baz :: Int }
  
  deriving instance (Show a) => Show (C99 a)
  deriving instance (Eq a) => Eq (C99 a)
  deriving instance (Ord a) => Ord (C99 a)
  deriving instance Functor C99
  deriving instance Foldable C99
  deriving instance Traversable C99
  instance ShowF C99 where showsPrecF = showsPrec
  instance EqF C99 where equalF = (==)
  instance IsString (C99 a) where fromString = Name
  instance OrdF C99 where compareF = compare
  
  deriving instance (IsString (f (Fix f))) => IsString (Fix f)
  
  -- 'nil'' isn't consistent with our naming conventions unfortunately, but I like it
  nil' = Fix Empty
  
  name' = Fix . Name
  -- TODO: investigate signed', unsigned', and int' - is this the most idiomatic way to express
  signed'   = Fix . Signed
  struct'   = Fix Struct
  union'    = Fix Union
  unsigned' = Fix . Unsigned
  
  short' = Fix . ShortM
  long'  = Fix . LongM
  
  function' nam typ args body = Fix $ Function nam typ args body
  
  arguments' l v = Fix $ Arguments l v
  
  void'          = Fix VoidT
  int'           = Fix IntT
  float'         = Fix FloatT
  double'        = Fix DoubleT
  char'          = Fix CharT
  verylong'      = Fix VeryLongT
  pointer_to'    = Fix . PointerToT
  builtin'       = Fix . BuiltinT
  array' len typ = Fix $ ArrayT len typ
  typedef_t'     = Fix . TypedefT
  bool'          = Fix BoolT 
  typeof'        = Fix . TypeOfT
  attribute' as t = Fix $ Attributed as t
  
  functionpointer' params returning = Fix $ FunctionPointerT params returning
  
  break'                    = Fix Break
  case' cond blk            = Fix $ Case cond blk -- this is specious, as are other case statements
  comma' a b                = Fix $ CommaSep a b
  continue'                 = Fix Continue
  default'                  = Fix . Default
  dowhile' cond blk         = Fix $ DoWhile cond blk
  for' var cond inc blk     = Fix $ For var cond inc blk
  goto'                     = Fix . Goto
  -- ifthen' cond blk          = Fix $ IfThen cond blk
  ifthenelse' cond blk blk2 = Fix $ IfThenElse cond blk blk2
  labeled' label stmt       = Fix $ Labeled label stmt
  return'                   = Fix . Return
  switch' cond blk          = Fix $ Switch cond blk
  while' cond blk           = Fix $ While cond blk
  
  cstr'                   = Fix . CStr
  cint'                   = Fix . CInt
  cfloat'                 = Fix . CFloat
  cchar'                  = Fix . CChar
  unary' op arg           = Fix $ Unary op arg
  binary' arg1 op arg2    = Fix $ Binary arg1 op arg2
  ternary' arg1 arg2 arg3 = Fix $ Ternary arg1 arg2 arg3
  cast' typ stmt          = Fix $ Cast typ stmt
  index' stmt idx         = Fix $ Index stmt idx
  call' stmt args         = Fix $ Call stmt args
  access' a b c           = Fix $ Access a b c
  vaarg' typ value        = Fix $ VaArg typ value
  paren'                  = Fix . Paren
  
  auto' = Fix . Auto
  const' = Fix . Const
  extern' = Fix . Extern
  inline' = Fix . Inline
  register' = Fix . Register
  restrict' = Fix . Restrict
  static' = Fix . Static
  volatile' = Fix . Volatile
  
  composite' kind nam fields = Fix $ Composite kind nam fields
  enumeration' nam vars = Fix $ Enumeration nam vars
  -- TODO fix these nomenclatures
  program' = Fix . Program
  group' = Fix . Group
  list' = Fix . List
  sized' t s = Fix $ Sized t s
  
  variable' a b c = tie $ Variable a b c -- TODO fix this nomenclature
  typedef' typ nam = tie $ Typedef typ nam
  
  
  
