{-# LANGUAGE UndecidableInstances #-}

module Semantics.C.ASG where
  
  import Control.Applicative hiding (Const)
  import Data.Fixed
  import Data.Functor.Fix
  import GHC.Exts (IsString (..))
  import Test.QuickCheck
  
  data Sem a where
    -- logical constructs
    Name     :: String -> Sem a -- used for binary operators as well as identifiers
    Signed   :: a -> Sem a
    Struct   :: Sem a
    Union    :: Sem a
    Unsigned :: a -> Sem a
    Variadic :: Sem a
    
    -- modifiers for types
    ShortM     :: a -> Sem a
    LongM      :: a -> Sem a
    
    Function :: { fname :: a, ftype :: a, fargs :: a, fbody :: a } -> Sem a 
      
    -- Attributed :: Attribute -> Anything -> Anything
    Attributed :: [a] -> a -> Sem a
  
    -- types
    VoidT            :: Sem a
    IntT             :: Sem a
    FloatT           :: Sem a
    DoubleT          :: Sem a
    CharT            :: Sem a
    ShortT           :: Sem a
    LongT            :: Sem a
    VeryLongT        :: Sem a
    PointerToT       :: a -> Sem a -- Pointer :: Type -> Type
    ArrayT           :: { array_length :: a, array_of :: a } -> Sem a
    FunctionPointerT :: a -> a -> Sem a
    BuiltinT         :: a -> Sem a -- Builtin :: Name -> Type
    TypedefT         :: a -> Sem a
    BoolT            :: Sem a
    TypeOfT          :: a -> Sem a
    
    -- statements
    Break      :: Sem a
    Case       :: a -> [a] -> Sem a
    CommaSep   :: a -> a -> Sem a
    Continue   :: Sem a
    Default    :: a -> Sem a
    DoWhile    :: a -> a -> Sem a
    Empty      :: Sem a
    For        :: a -> a -> a -> a -> Sem a
    Goto       :: a -> Sem a
    IfThen     :: a -> a -> Sem a
    IfThenElse :: a -> a -> a -> Sem a
    Labeled    :: a -> a -> Sem a
    Return     :: a -> Sem a
    Switch     :: a -> [a] -> Sem a -- Buggy: should have a Group as its second parameter
    While      :: a -> a -> Sem a
  
    -- expressions
    CStr     :: String -> Sem a
    CInt     :: Integer -> Sem a
    CFloat   :: Nano -> Sem a
    CChar    :: Char -> Sem a
    Unary    :: a -> a -> Sem a
    Binary   :: a -> a -> a -> Sem a
    Ternary  :: a -> a -> a -> Sem a
    Cast     :: [a] -> a -> Sem a
    Brackets :: a -> a -> Sem a -- rename to Index?
    FunCall  :: a -> [a] -> Sem a -- rename to Call?
    VaArg    :: a -> a -> Sem a
    Paren    :: a -> Sem a
  
    -- attributes
    Auto     :: a -> Sem a
    Const    :: a -> Sem a
    Extern   :: a -> Sem a
    Inline   :: a -> Sem a
    Register :: a -> Sem a
    Restrict :: a -> Sem a
    Static   :: a -> Sem a
    Volatile :: a -> Sem a
    Custom   :: [a] -> a -> Sem a
  
    -- other stuff
    Enumeration :: { ename :: a, emembers :: a } -> Sem a
    Prototype :: { pname :: a, ptype :: a, pargs :: a } -> Sem a -- we should be able to get rid of this and just have a Function with a nil' body
    Composite :: { ckind :: a, cname :: a, cfields :: a } -> Sem a
    Program :: [a] -> Sem a
    Group :: [a] -> Sem a
    List  :: [a] -> Sem a -- do we use this anywhere?
    Assembly :: { avolatile :: Bool, atext :: a, ainregs :: a, aoutregs :: a, aclobber :: a } -> Sem a
    AssemblyOperand :: { opconstraint :: a, opvar :: a } -> Sem a
    
    Arguments :: [a] -> Bool -> Sem a
    ForwardTypeDeclaration :: a -> Sem a
    Variable :: { vtype :: a, vname :: a, vvalue :: a } -> Sem a
    Typedef :: { ttype :: a, tname :: a } -> Sem a
    Sized :: a -> a -> Sem a
  
  deriving instance (Show a) => Show (Sem a)
  deriving instance (Eq a) => Eq (Sem a)
  deriving instance (Ord a) => Ord (Sem a)
  deriving instance Functor Sem
  deriving instance Foldable Sem
  deriving instance Traversable Sem
  instance ShowF Sem where showsPrecF = showsPrec
  instance EqF Sem where equalF = (==)
  instance IsString (Sem a) where fromString = Name
  instance OrdF Sem where compareF = compare
  
  deriving instance (IsString (f (Fix f))) => IsString (Fix f)
  
  -- 'nil'' isn't consistent with our naming conventions unfortunately, but I like it
  nil' = Fix Empty
  
  name' = Fix . Name
  -- TODO: investigate signed', unsigned', and int' - is this the most idiomatic way to express
  signed'   = Fix . Signed
  struct'   = Fix Struct
  union'    = Fix Union
  unsigned' = Fix . Unsigned
  -- unsigned' = Fix Unsigned
  variadic' = Fix Variadic
  
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
  
  functionpointer' params returning = Fix $ FunctionPointerT params returning
  
  break'                    = Fix Break
  case' cond blk            = Fix $ Case cond blk -- this is specious, as are other case statements
  comma' a b                = Fix $ CommaSep a b
  continue'                 = Fix Continue
  default'                  = Fix . Default
  dowhile' cond blk         = Fix $ DoWhile cond blk
  for' var cond inc blk     = Fix $ For var cond inc blk
  goto'                     = Fix . Goto
  ifthen' cond blk          = Fix $ IfThen cond blk
  ifthenelse' cond blk blk2 = Fix $ IfThenElse cond blk blk2
  labeled' label stmt       = Fix $ Labeled label stmt
  return'                   = Fix . Return
  switch' cond blk          = Fix $ Switch cond blk
  while' cond blk           = Fix $ While cond blk
  
  str'                    = Fix . CStr
  cint'                   = Fix . CInt
  cfloat                  = Fix . CFloat
  cchar'                  = Fix . CChar
  unary' op arg           = Fix $ Unary op arg
  binary' arg1 op arg2    = Fix $ Binary arg1 op arg2
  ternary' arg1 arg2 arg3 = Fix $ Ternary arg1 arg2 arg3
  cast' typ stmt          = Fix $ Cast typ stmt
  brackets' stmt idx      = Fix $ Brackets stmt idx
  funcall' stmt args      = Fix $ FunCall stmt args
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
  custom' t a = Fix $ Custom t a
  
  prototype' nam typ args = Fix $ Prototype nam typ args
  composite' kind nam fields = Fix $ Composite kind nam fields
  enumeration' nam vars = Fix $ Enumeration nam vars
  -- TODO fix these nomenclatures
  program' = Fix . Program
  group' = Fix . Group
  list' = Fix . List
  sized' t s = Fix $ Sized t s
  
  variable' a b c = tie $ Variable a b c -- TODO fix this nomenclature
  typedef' typ nam = tie $ Typedef typ nam
  
  type CSem = forall a. Sem a
  type FSem = Fix Sem

  instance Arbitrary FSem where
    arbitrary = undefined



  isGroup :: CSem -> Bool
  isGroup (Group _) = True
  isGroup _ = False
  
  
