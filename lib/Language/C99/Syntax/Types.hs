module Language.C99.Syntax.Types where
  
  import Data.Comp
  import Data.Comp.Derive

  data Type a where
    -- The core C types, prefixed so as to avoid name collisions. Corresponds to C99 6.7.2, type specifiers.
    CVoid            :: Type a
    CInt             :: Type a
    CFloat           :: Type a
    CDouble          :: Type a
    CChar            :: Type a
    CBool            :: Type a
    CInt128          :: Type a
    CBuiltin          :: { name :: a } -> Type a
    
    -- "Derived" types: pointers, arrays, typedefs, and attributes.
    Pointer    :: { typ :: a } -> Type a
    Array      :: { typ :: a, len :: a } -> Type a
    Typedef    :: { typ :: a, name :: a } -> Type a
    Attributed :: { typ :: a, attr :: a } -> Type a
    
    -- Composite types: structs, unions, and enums.
    Struct           :: Type a
    Union            :: Type a
    Composite        :: { kind :: a, name :: a, members :: a } -> Type a
    Enumeration      :: { name :: a, members :: a } -> Type a
    
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
         , smartConstructors] [''Type]
  