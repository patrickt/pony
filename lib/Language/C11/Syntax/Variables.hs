module Language.C11.Syntax.Variables 
  where
  
  import Language.Pony.Overture
  import Data.Comp.Derive
  import Control.Lens
  import StringTable.Atom
  
  import Language.C11.Syntax.Literals
  import Language.C11.Syntax.Lens
  
  data Variable a = Variable 
    { _variableType :: a
    , _variableName :: Name
    , _variableInitializer :: a 
    } deriving (Show, Eq)
  
  data Sized a = Sized 
    { _sTarget :: a
    , _sSize :: a }
      
  derive [ makeShowF
         , makeEqF
         , makeFunctor
         , makeFoldable
         , makeLenses
         , makeTraversable
         , smartConstructors] [ ''Variable
                              , ''Sized 
                              ]
                              
  instance HasName (Variable a) where name = variableName
  instance HasType Variable where typ = variableType
  