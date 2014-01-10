module Language.C11.Syntax.Constructs where
  
  import Control.Lens
  import Control.Lens.TH
  import Data.Comp
  import Data.Comp.Derive
  import Data.Comp.Variables
  import StringTable.Atom
  
  import Language.C99.Syntax.Lens
  
  data Construct a where
    Function :: { _ofType :: a, _name :: Atom, _arguments :: [a], _body :: [a] } -> Construct a
    Declaration :: { _variable :: a, _initializer :: a } -> Construct a
    Program :: { _body :: [a] } -> Construct a
    
  derive [ makeShowF
         , makeEqF
         , makeFunctor
         , makeFoldable
         , makeTraversable
         , smartConstructors] [''Construct]
  
  
  instance HasType Construct where ofType = lens _ofType (\it t -> it { _ofType = t })
  instance HasName Construct where name = lens _name (\it t -> it { _name = t })
  instance HasArguments Construct where arguments = lens _arguments (\it t -> it { _arguments = t })
  instance HasBody Construct where body = lens _body (\it t -> it { _body = t })
  