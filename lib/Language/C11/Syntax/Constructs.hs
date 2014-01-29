module Language.C11.Syntax.Constructs where
  
  import Language.Pony.Overture
  import Control.Lens
  import Control.Lens.TH
  import Data.Comp
  import Data.Comp.Derive
  import Data.Comp.Variables
  import StringTable.Atom
  
  import Language.C11.Syntax.Lens
  
  newtype Program a = Program { _body :: [a] }
    deriving (Show, Eq)
  
  data ForwardDeclaration a = ForwardDeclaration { _target :: a }
    deriving (Show, Eq)
    
  data Typedef a = Typedef { _typ :: a, _name :: ByteString }
    deriving (Show, Eq)
    
  data Empty a = Empty deriving (Show, Eq)
  
  derive [ makeShowF
         , makeEqF
         , makeFunctor
         , makeFoldable
         , makeTraversable
         , smartConstructors] [ ''Program
                              , ''ForwardDeclaration
                              , ''Typedef
                              , ''Empty
                              ]
  
  instance HasBody Program where body = lens _body (\it t -> it { _body = t })
  instance HasTarget ForwardDeclaration where target = lens _target (\it t -> it { _target = t })
  instance HasName Typedef where name = lens _name (\it t -> it { _name = t })
  instance HasType Typedef where typ = lens _typ (\it t -> it { _typ = t })