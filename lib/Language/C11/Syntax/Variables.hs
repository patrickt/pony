module Language.C11.Syntax.Variables 
  where
  
  import Language.Pony.Overture
  import Data.Comp.Derive
  import Control.Lens
  import StringTable.Atom
  
  import Language.C11.Syntax.Lens
  
  data Variable a = Variable 
    { _vType :: a
    , _vName :: ByteString
    , _vInitializer :: a 
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
         
  instance HasType Variable where typ = vType
  instance TravName Variable where nameT = vName
  instance HasName Variable where name = vName
  