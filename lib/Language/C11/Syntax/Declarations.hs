module Language.C11.Syntax.Declarations where
  
  import Language.Pony.Overture
  
  import Control.Lens
  import Data.Coproduct
  
  data Decl a where
    FunDecl :: { _declFunction :: a, _declBody :: [a] } -> Decl a
    CompDecl :: { _declType :: a, _declFields :: [a] } -> Decl a
    EnumDecl :: { _declType :: a, _declMembers :: [a] } -> Decl a
    ForwardDecl :: { _declType :: a } -> Decl a
  
  deriving instance (Show a) => Show (Decl a)
  
  derive [ makeShowF
         , makeEqF
         , makeFunctor
         , makeFoldable
         , makeTraversable
         , makePrisms
         , makeLenses
         , smartConstructors] [ ''Decl 
                              ]