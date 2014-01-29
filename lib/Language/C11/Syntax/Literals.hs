{-# OPTIONS_GHC -ddump-splices #-}

module Language.C11.Syntax.Literals 
  ( 
  -- * Data types
    IntLit (..)
  , FltLit (..)
  , StrLit (..)
  , ChrLit (..)
  , Ident (..)
  , Constant
  , Literal
  -- * Reexported Lenses
  , name
  , base
  , value
  -- * Isomorphisms
  , strLit
  , chrLit
  , ident
  -- * Smart (injective) constructors
  , iIntLit
  , iFltLit
  , iChrLit
  , iStrLit
  , iIdent
  )

  where
  
  import Language.Pony.Overture
  import Data.Comp
  import Data.Comp.Derive
  import Data.Text
  import Data.String
  import Control.Lens
  import Data.ByteString.Lens
  import StringTable.Atom
  
  import Language.C11.Syntax.Types (CType)
  import Language.C11.Syntax.Lens
  
  import Data.Scientific as Scientific
  
  -- | C integer literals.
  data IntLit a = IntLit {
     -- | Lensed with 'value'.
     _intValue :: Integer, 
     -- Lensed with 'base'.
     _intBase :: Int,
     -- | Corresponds to the suffix, if any, of the literal in question.
     -- In the future this field may be of type 'Term' 'CType' or @a@.
     _intSuffix :: Maybe ByteString 
  } deriving (Show, Eq, Functor, Foldable, Traversable)
  
  -- | As with 'IntLit' literals, but storing a 'Scientific' instead of an integer.
  data FltLit a = FltLit {
    -- Lensed with 'value'.
    _fltValue :: Scientific, 
    -- Lensed with 'base'.
    _fltBase :: Int, 
    _fltSuffix :: Maybe ByteString 
  } deriving (Show, Eq, Functor, Foldable, Traversable)
  
  -- | Character literals.
  newtype ChrLit a = ChrLit { 
    -- | Lensed with 'value'.
    _chrValue :: Char 
  } deriving (Show, Eq, Functor, Foldable, Traversable)
  
  -- | String literals. In the future these may be represented with an 'Atom' behind the scenes.
  newtype StrLit a = StrLit { 
    -- | Lensed with 'value'.
    _strValue :: ByteString
  } deriving (Show, Eq, Functor, Foldable, Traversable, Monoid, IsString, IsByteString)
    
  
  -- | Identifiers. In the future these may be represented with an 'Atom' behind the scenes.
  newtype Ident a = Ident { 
    -- | Lensed with 'name'.
    _identName :: ByteString
  } deriving (Show, Eq, Functor, Foldable, Traversable, Monoid, IsString, IsByteString)
    
  
  -- | The type of constants, as per C99 6.4.4.
  type Constant = IntLit :+: FltLit :+: ChrLit
  
  -- | The type of all literals. Included for convenience/clarity.
  type Literal  = StrLit :+: Constant
       
  derive [ makeShowF
         , makeEqF
         , smartConstructors] [ ''IntLit
                              , ''FltLit
                              , ''StrLit
                              , ''ChrLit
                              , ''Ident
                              ]
  
                              
  derive [ makeLenses ] [ ''IntLit, ''FltLit ]
  derive [ makeIso ] [ ''StrLit, ''ChrLit, ''Ident ]
  
                              
  -- | Please be aware that this is not a lawful Num instance, as it does not respect 
  -- information about the base and suffix in any way. Regardless, it is included in order 
  -- to allow the use of Haskell integer literals where an 'IntLit' is expected.
  instance Num (IntLit a) where
    fromInteger i = IntLit i 10 Nothing
    (+) a b = a & intValue +~ b^.intValue
    (*) a b = a & intValue *~ b^.intValue
    negate = intValue %~ negate
    abs = intValue %~ abs
    signum = intValue %~ signum
  
  instance HasValue IntLit Integer where value = intValue
  instance HasValue FltLit Scientific where value = fltValue
  instance HasValue StrLit ByteString where value = strValue
  instance HasValue ChrLit Char where value = chrValue
  
  instance HasBase IntLit where base = intBase
  instance HasBase FltLit where base = fltBase
  
  instance TravName Ident where nameT = identName 
  instance HasName Ident where name = identName
  

