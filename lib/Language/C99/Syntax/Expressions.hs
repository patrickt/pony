module Language.C99.Syntax.Expressions where
  
  import Data.Comp.Derive
  
  data Expr a where
    Comma    :: a -> a -> Expr a
    Unary    :: { op :: a, arg :: a } -> Expr a
    Binary   :: { op :: a, left :: a, right :: a } -> Expr a
    Ternary  :: a -> a -> a -> Expr a
    Cast     :: { typ :: a, arg :: a} -> Expr a
    Index    :: a -> a -> Expr a
    Call     :: a -> [a] -> Expr a
    Access   :: a -> a -> a -> Expr a
    Paren    :: a -> Expr a
  
  derive [ makeShowF
         , makeEqF
         , makeFunctor
         , makeFoldable
         , makeTraversable
         , smartConstructors] [''Expr]