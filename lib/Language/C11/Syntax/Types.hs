module Language.C11.Syntax.Types where
  
  import Control.Lens
  import Data.Comp.Derive
  import StringTable.Atom
  
  import Language.C11.Syntax.Lens
  
  data Struct a = Struct deriving (Show, Eq)
  data Union a = Union deriving (Show, Eq)
  data Enumeration a = Enum deriving (Show, Eq)
  
  data Composite a = Composite 
    { _kind :: a
    , _name :: Atom
    , _members :: [a] 
    } deriving (Show, Eq)
    
  makeLensesFor [("_kind", "kind"), ("_members", "members")] ''Composite
  instance HasName Composite where name = lens _name (\it t -> it { _name = t })
  
  data Attributed a = Attributed 
    { _target :: a
    , _attrs :: [a] 
    } deriving (Show, Eq)
  
  makeLensesFor [("attrs", "_attrs")] ''Attributed
  instance HasTarget Attributed where target = lens _target (\it t -> it { _target = t })
  
  data Type a where
    -- The core C types, prefixed so as to avoid name collisions. Corresponds to C99 6.7.2, type specifiers.
    CVoid            :: Type a
    CInt             :: Type a
    CFloat           :: Type a
    CDouble          :: Type a
    CChar            :: Type a
    CBool            :: Type a
    CInt128          :: Type a
    CBuiltin         :: Atom -> Type a
    
    -- "Derived" types: pointers, arrays, typedefs, and attributes.
    Pointer    :: { _innerType :: a} -> Type a
    Array      :: { _innerType :: a, _size :: a } -> Type a
        
    -- Type modifiers, each of which wraps an "inner" type. 
    -- These are intended to be human-readable and read left-to-right, in contrast with how C types are actually printed.
    -- Examples (cdecl.org will help you with these):
    --  const char x; => Const CChar
    --  const char * x; => Pointer (Const Char)
    --  const char * const x; => Const (Pointer (Const Char))
    --  const char * const * x; => PointerTo (Const (Pointer (Const Char)))
    -- And so on.
    Signed    :: a -> Type a
    Unsigned  :: a -> Type a
    Short     :: a -> Type a
    Long      :: a -> Type a
    
    Auto     :: a -> Type a
    Const    :: a -> Type a
    Extern   :: a -> Type a
    Inline   :: a -> Type a
    Register :: a -> Type a
    Restrict :: a -> Type a
    Static   :: a -> Type a
    Volatile :: a -> Type a
  
  derive [ makeShowF
         , makeEqF
         , makeFunctor
         , makeFoldable
         , makeTraversable
         , smartConstructors] [ ''Struct
                              , ''Union]
  