module Language.C11.Syntax.Operators where
  
  import Data.Comp.Derive
  import Data.Comp.Show
  
  data Operator a where -- TODO: so many name collisions about this
    Add :: Operator a
    Sub :: Operator a
    Mul :: Operator a
    Div :: Operator a
    Mod :: Operator a
    Inc :: Operator a
    Dec :: Operator a
    Not :: Operator a
    Assign :: Operator a
    Equal :: Operator a
    NotEqual :: Operator a
    
    And :: Operator a
    Or  :: Operator a
    XOr :: Operator a
    
    Neg :: Operator a
    Pos :: Operator a
    LShift :: Operator a
    RShift :: Operator a
    SizeOf :: Operator a
    
    Ref :: Operator a
    Deref :: Operator a
    
    PostInc :: Operator a
    PostDec :: Operator a
    Bitwise :: Operator a -> Operator a
    WithAssignment :: Operator a -> Operator a
    
  deriving instance (Show a) => Show (Operator a)
  deriving instance (Eq a) => Eq (Operator a)

  derive [ makeShowF
         , makeEqF
         , makeFunctor
         , makeFoldable
         , makeTraversable
         , smartConstructors] [''Operator]