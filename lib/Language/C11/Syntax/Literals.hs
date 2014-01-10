module Language.C11.Syntax.Literals where
  
  import Data.Comp.Derive
  import Data.Text
  import Control.Lens
  import StringTable.Atom
  
  import Language.C99.Syntax.Lens
  
  import Data.Scientific as Scientific
  
  data Literal f a where
    IntLiteral :: { _integerValue :: Integer, _suffix :: a } -> Literal Integer a
    DecLiteral :: { _decimalValue :: Scientific } -> Literal Scientific a
    StrLiteral :: { _stringValue  :: Text } -> Literal Text a
    ChrLiteral :: { _charValue    :: Char } -> Literal Char a
  
  -- Todo: determine when an Ident node is necessary and when a name node of type Atom is okay.
  data Ident a = Ident { _name :: Atom }
      
  derive [ makeShowF
         , makeEqF
         , makeFunctor
         , makeFoldable
         , makeTraversable
         , smartConstructors] [''Literal, ''Ident]
         
  

