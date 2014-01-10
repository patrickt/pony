module Language.C99.Syntax.Statements where
  
  import Data.Comp
  import Data.Comp.Derive
  
  data Stmt a where
    Break      :: Stmt a
    Case       :: a -> a -> Stmt a
    Continue   :: Stmt a
    Compound   :: [a] -> Stmt a
    Default    :: a -> Stmt a
    DoWhile    :: a -> a -> Stmt a
    For        :: [a] -> a -> a -> a -> Stmt a
    Goto       :: a -> Stmt a
    IfThenElse :: a -> a -> Maybe a -> Stmt a
    Labeled    :: a -> a -> Stmt a
    Return     :: a -> Stmt a
    Switch     :: a -> a -> Stmt a
    While      :: a -> a -> Stmt a
    
    Assembly   :: { isVolatile :: Bool, text :: a, inRegs :: [a], outRegs :: [a], clobberList :: [a] } -> Stmt a
  
  derive [ makeShowF
         , makeEqF
         , makeFunctor
         , makeFoldable
         , makeTraversable
         , smartConstructors] [''Stmt]