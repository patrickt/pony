module Language.C11.Syntax.Constructs where
  
  import Language.Pony.Overture
  import Control.Lens
  import Control.Lens.TH
  import Data.Comp
  import Data.Comp.Derive
  import Data.Comp.Variables
  import StringTable.Atom
  
  import Language.C11.Syntax.Lens
  
  newtype Program a = Program { _programBody :: [a] }
    deriving (Show, Eq)
  
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
         , smartConstructors] [ ''Program
                              , ''ForwardDeclaration
                              , ''Typedef
                              , ''Empty
                              , ''Attribute
                              ]
  
  instance HasBody Program where body = programBody
  instance HasTarget ForwardDeclaration where target = forwardTarget
  
  instance TravName Typedef where nameT = typedefName
  instance HasName Typedef where name = typedefName
  instance HasType Typedef where typ = typedefType
  