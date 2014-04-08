{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.C11.Syntax.Literals

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
  
  derive [ makeShowF, makeEqF, makePrisms ] [ ''Literal ]
  
  iIntLit :: (Literal :<: f) => Integer -> Int -> Maybe ByteString -> Term f
  iIntLit v b s = inject $ IntLit $ Numeric v b s
  
  iChrLit :: (Literal :<: f) => Char -> Term f
  iChrLit = inject . ChrLit
  
  iFltLit :: (Literal :<: f) => Scientific -> Int -> Maybe ByteString -> Term f
  iFltLit v b s = inject $ FltLit $ Numeric v b s
    
  newtype Ident a = Ident Name
    deriving (Show, Eq, Functor, Foldable, Traversable, Monoid, HasName, IsString, IsByteString)
       
  smartConstructors ''Ident
