module Language.C11.Syntax.Variables 
  where
  
  import Language.Pony.Overture
  import Data.Comp.Derive
  import Control.Lens
  import StringTable.Atom
  
  import Language.C11.Syntax.Lens
  
  data Variable a = Variable 
    { _typ :: a
    , _name :: ByteString
    , _initializer :: a 
    } deriving (Show, Eq)
  
  data Sized a = Sized 
    { _target :: a
    , _size :: a }
      
  derive [ makeShowF
         , makeEqF
         , makeFunctor
         , makeFoldable
         , makeTraversable
         , smartConstructors] [ ''Variable
                              , ''Sized 
                              ]
         
  instance HasType Variable where typ = lens _typ (\it t -> it { _typ = t })
  instance HasName Variable where name = lens _name (\it t -> it { _name = t })
  