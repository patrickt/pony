{-# LANGUAGE UndecidableInstances #-}

module Language.C11.Syntax.Constructs where
  
  import Language.Pony.Overture
  import Control.Lens
  import Control.Lens.TH
  import Data.Comp
  import Data.Comp.Derive
  import Data.Comp.Variables
  import StringTable.Atom
  
  import Language.C11.Syntax.Lens
  
  newtype TranslationUnit a = TranslationUnit [a]
    deriving (Show, Eq, Monoid, Default)
    
  makeWrapped ''TranslationUnit
  
  data ForwardDeclaration a = ForwardDeclaration { _forwardTarget :: a }
    deriving (Show, Eq)
    
  data Typedef a = Typedef { _typedefType :: a, _typedefName :: ByteString }
    deriving (Show, Eq)
  
  data Attribute a where 
    CAttribute :: { _attrContents :: [a] } -> Attribute a
    AsmName   :: { _attrName :: ByteString } -> Attribute a
    
  data Empty a = Empty deriving (Show, Eq)
  
  derive [ makeShowF
         , makeEqF
         , makeFunctor
         , makeFoldable
         , makeTraversable
         , makePrisms
         , makeLenses
         , smartConstructors] [ ''TranslationUnit
                              , ''ForwardDeclaration
                              , ''Typedef
                              , ''Empty
                              , ''Attribute
                              ]
  
  instance HasTarget ForwardDeclaration where target = forwardTarget
  
  instance TravName Typedef where nameT = typedefName
  instance HasType Typedef where typ = typedefType
  