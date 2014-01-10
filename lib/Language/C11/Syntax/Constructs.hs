module Language.C11.Syntax.Constructs where
  
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
    
  data Empty a = Empty deriving (Show, Eq)
  
  derive [ makeShowF
         , makeEqF
         , makeFunctor
         , makeFoldable
         , makeTraversable
         , smartConstructors] [ ''Program
                              , ''ForwardDeclaration
                              , ''Empty
                              ]
  
  instance HasBody Program where body = lens _body (\it t -> it { _body = t })
  instance HasTarget ForwardDeclaration where target = lens _target (\it t -> it { _target = t })
  