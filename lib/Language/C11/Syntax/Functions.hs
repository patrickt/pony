module Language.C11.Syntax.Functions where
  
  import Control.Lens
  import Data.Comp.Derive
  import StringTable.Atom
  
  import Language.C11.Syntax.Lens
  
  data Function a = Function 
    { _typ :: a
    , _name :: Atom
    , _arguments :: [a]
    , _body :: [a] 
    } deriving (Show, Eq)
    
  data Variadic a = Variadic deriving (Show, Eq)
  
  derive [ makeShowF
         , makeEqF
         , makeFunctor
         , makeFoldable
         , makeTraversable
         , smartConstructors] [ ''Function
                              , ''Variadic
                              ]
  
  instance HasType Function where typ = lens _typ (\it t -> it { _typ = t })
  instance HasName Function where name = lens _name (\it t -> it { _name = t })
  instance HasArguments Function where arguments = lens _arguments (\it t -> it { _arguments = t })
  instance HasBody Function where body = lens _body (\it t -> it { _body = t })
  
  