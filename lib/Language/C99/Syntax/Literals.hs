module Language.C99.Syntax.Literals where
  
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
      
  derive [ makeShowF
         , makeEqF
         , makeFunctor
         , makeFoldable
         , makeTraversable
         , smartConstructors] [''Literal]
         
  

