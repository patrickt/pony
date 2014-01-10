module Language.C11.Syntax.Literals where
  
  import Data.Comp.Derive
  import Data.Text
  import Control.Lens
  import StringTable.Atom
  
  import Language.C11.Syntax.Lens
  
  import Data.Scientific as Scientific
  
  data Literal a where
    IntLiteral :: { _integerValue :: Integer, _suffix :: a } -> Literal a
    DecLiteral :: { _decimalValue :: Scientific } -> Literal a
    StrLiteral :: { _stringValue  :: Text } -> Literal a
    ChrLiteral :: { _charValue    :: Char } -> Literal a
  
  newtype Ident a = Ident { _name :: Atom }
      
  derive [ makeShowF
         , makeEqF
         , makeFunctor
         , makeFoldable
         , makeTraversable
         , smartConstructors] [ ''Literal
                              , ''Ident
                              ]
  
  makeLenses ''Literal
  
  instance HasName Ident where name = lens _name (\it t -> it { _name = t })
         
  

