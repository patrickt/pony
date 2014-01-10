module Language.C99.Syntax.Operators where
  
  import Data.Comp.Derive
  import Data.Comp.Show
  
  data COperator a where -- TODO: so many name collisions about this
    Add :: COperator a
    Sub :: COperator a
    Mul :: COperator a
    Div :: COperator a
    Mod :: COperator a
    Inc :: COperator a
    Dec :: COperator a
    Not :: COperator a
    Assign :: COperator a
    Equal :: COperator a
    NotEqual :: COperator a
    
    And :: COperator a
    Or  :: COperator a
    XOr :: COperator a
    
    Negate :: COperator a
    LShift :: COperator a
    RShift :: COperator a
    SizeOf :: COperator a
    
    PostInc :: COperator a
    PostDec :: COperator a
    Bitwise :: a -> COperator a
    WithAssignment :: a -> COperator a

  derive [ makeShowF
         , makeEqF
         , makeFunctor
         , makeFoldable
         , makeTraversable
         , smartConstructors] [''COperator]