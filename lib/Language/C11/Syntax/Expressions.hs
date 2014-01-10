module Language.C11.Syntax.Expressions where
  
  import Data.Comp.Derive
  
  data Expr a where
    Unary    :: { _op :: a, _target :: a } -> Expr a
    Binary   :: { _op :: a, _left :: a, _right :: a } -> Expr a
    Ternary  :: { _condition :: a, _left :: a, _right :: a} -> a -> a -> Expr a
    Cast     :: { _typeOf :: a, _target :: a} -> Expr a
    Index    :: { _target :: a, _index :: a } -> Expr a
    Call     :: { _target :: a, _arguments :: [a] } -> [a] -> Expr a
    Access   :: { _target :: a, _op :: a, _member :: a } -> Expr a
    Paren    :: { _target :: a } -> Expr a
  
  derive [ makeShowF
         , makeEqF
         , makeFunctor
         , makeFoldable
         , makeTraversable
         , smartConstructors] [''Expr]