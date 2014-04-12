module Language.C11.Syntax.Expressions where

  import Language.Pony.Overture
  import Control.Lens  
  import Control.Lens.TH
  import Data.Comp.Derive
  import Language.C11.Syntax.Lens
  import Language.C11.Syntax.Operators
  
  data Expr a where
    Unary    :: { _operation :: a, _target :: a } -> Expr a
    Binary   :: { _operation :: a, _left :: a, _right :: a } -> Expr a
    Ternary  :: { _condition :: a, _left :: a, _right :: a} -> Expr a
    Cast     :: { _left :: a, _right :: a} -> Expr a
    Index    :: { _left :: a, _right :: a } -> Expr a
    Call     :: { _target :: a, _arguments :: [a] } -> Expr a
    Dot      :: { _target :: a, _identifier :: a } -> Expr a
    Arrow    :: { _target :: a, _identifier :: a } -> Expr a
    Paren    :: { _target :: a } -> Expr a
  
  derive [ makeShowF
         , makeEqF
         , makeFunctor
         , makeFoldable
         , makeTraversable
         , makeLensesFor [ ("_operation", "operation")
                         , ("_left", "left")
                         , ("_right", "right")
                         , ("_condition", "condition")]
         , makePrisms
         , smartConstructors] [''Expr]
  
  instance HasArguments Expr where arguments = lens _arguments (\it t -> it { _arguments = t })
  instance HasTarget Expr where target = lens _target (\it t -> it { _target = t })
  
