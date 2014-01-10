module Language.C11.Syntax.Statements where
  
  import Control.Lens
  import Data.Comp.Derive
  
  import Language.C11.Syntax.Lens
  
  data Stmt a where
    Break      :: Stmt a
    Case       :: a -> a -> Stmt a
    Continue   :: Stmt a
    Compound   :: { _body :: [a] } -> Stmt a
    Default    :: a -> Stmt a
    DoWhile    :: a -> a -> Stmt a
    For        :: [a] -> a -> a -> a -> Stmt a
    Goto       :: a -> Stmt a
    IfThenElse :: a -> a -> Maybe a -> Stmt a
    Labeled    :: a -> a -> Stmt a
    Return     :: a -> Stmt a
    Switch     :: a -> a -> Stmt a
    While      :: a -> a -> Stmt a
  
  derive [ makeShowF
         , makeEqF
         , makeFunctor
         , makeFoldable
         , makeTraversable
         , smartConstructors] [''Stmt]
         
  instance HasBody Stmt where body = lens _body (\it t -> it { _body = t })