module Language.C11.Syntax.Types where
  
  import Language.Pony.Overture
  
  import Control.Lens hiding (Const)
  import Data.Comp.Derive
  import StringTable.Atom
  
  import Language.C11.Syntax.Lens
  
  data Struct a = Struct deriving (Show, Eq)
  data Union a = Union deriving (Show, Eq)
  data Enumeration a = Enum deriving (Show, Eq)
  
  data Composite a = Composite 
    { _cKind :: a
    , _cName :: ByteString
    , _cMembers :: [a] 
    } deriving (Show, Eq)
    
  makeLenses ''Composite
  instance TravName Composite where nameT = cName
  
  data Attributed a = Attributed 
    { _target :: a
    , _attrs :: [a] 
    } deriving (Show, Eq)
  
  makeLensesFor [("attrs", "_attrs")] ''Attributed
  instance HasTarget Attributed where target = lens _target (\it t -> it { _target = t })
  
  data CType a where
    -- The core C types, prefixed so as to avoid name collisions. Corresponds to C99 6.7.2, type specifiers.
    CVoid            :: CType a
    CInt             :: CType a
    CFloat           :: CType a
    CDouble          :: CType a
    CChar            :: CType a
    CBool            :: CType a
    CInt128          :: CType a
    CBuiltin         :: Atom -> CType a
    Variadic         :: CType a
    
    -- "Derived" types: pointers, arrays, typedefs, and attributes.
    Pointer    :: { _typ :: a} -> CType a
    Array      :: { _typ :: a, _size :: a } -> CType a
        
    -- CType modifiers, each of which wraps an "inner" type. 
    -- These are intended to be human-readable and read left-to-right, in contrast with how C types are actually printed.
    -- Examples (cdecl.org will help you with these):
    --  const char x; => Const CChar
    --  const char * x; => Pointer (Const Char)
    --  const char * const x; => Const (Pointer (Const Char))
    --  const char * const * x; => PointerTo (Const (Pointer (Const Char)))
    -- And so on.
    Signed    :: a -> CType a
    Unsigned  :: a -> CType a
    Short     :: a -> CType a
    Long      :: a -> CType a
    
    Auto     :: a -> CType a
    Const    :: a -> CType a
    Extern   :: a -> CType a
    Inline   :: a -> CType a
    Register :: a -> CType a
    Restrict :: a -> CType a
    Static   :: a -> CType a
    Volatile :: a -> CType a
    
  deriving instance (Eq a) => Eq (CType a)
  deriving instance (Show a) => Show (CType a)
  
  derive [ makeShowF
         , makeEqF
         , makeFunctor
         , makeFoldable
         , makeTraversable
         , smartConstructors] [ ''Struct
                              , ''Union
                              , ''CType
                              ]
  
  size :: Lens' (CType a) a
  size = lens _size (\it t -> it { _size = t })
  
  
  instance HasType CType where typ = lens _typ (\it t -> it { _typ = t })