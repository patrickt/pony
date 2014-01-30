{-# OPTIONS_GHC -ddump-splices #-}

module Language.C11.Syntax.Literals 
<<<<<<< HEAD
=======
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
  -- * Prisms
  , _IntLit
  , _FltLit
  , _ChrLit
  , _StrLit
  , _Ident
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
>>>>>>> prisms

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
  
  data Numeric f = Numeric 
    { _number :: f
    , _base :: Int
    , _suffix :: Maybe ByteString 
    } deriving (Eq, Show, Functor, Foldable, Traversable)
    
  makeLenses ''Numeric
  
  newtype Name = Name ByteString
    deriving (Show, Eq, Monoid, IsString, IsByteString)
  
  derive [ makeClassy, makeWrapped ] [''Name]
  
  data Literal a 
    = IntLit (Numeric Integer)
    | FltLit (Numeric Scientific)
    | ChrLit Char
    | StrLit ByteString
    deriving (Show, Eq, Functor, Foldable, Traversable)
  
  makePrisms ''Literal
    
  newtype Ident a = Ident Name
    deriving (Show, Eq, Functor, Foldable, Traversable, Monoid, HasName, IsString, IsByteString)
    
  makeWrapped ''Ident
       
  derive [ makeShowF
         , makeEqF
         , smartConstructors] [ ''Literal
                              , ''Ident
                              ]

  

