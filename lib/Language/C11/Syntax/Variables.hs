module Language.C11.Syntax.Variables 
  where
  
  import Data.Comp.Derive
  import Control.Lens
  import StringTable.Atom
  
  import Language.C99.Syntax.Lens
  
  data Variable a where
    Variable :: { _ofType :: a, _name :: Atom } -> Variable a
      
  derive [ makeShowF
         , makeEqF
         , makeFunctor
         , makeFoldable
         , makeTraversable
         , smartConstructors] [''Variable]
         
  instance HasType Variable where ofType = lens _ofType (\it t -> it { _ofType = t })
  instance HasName Variable where name = lens _name (\it t -> it { _name = t })
  