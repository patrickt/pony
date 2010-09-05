{-# LANGUAGE UnicodeSyntax, TypeFamilies, GADTs #-}

module Language.Pony.Node where
  
  import Language.C.Data.Node
  import Language.C.Syntax.AST
  import Language.C.Syntax.Constants
    
  -- The Concrete data family represents things that can be compiled down to CNodes.
  -- At the end of all of the transformations, every node in the syntax tree must 
  -- be a Concrete node. The ConcreteRep type is necessary 
  class Concrete a where
    type CNodelike :: *
    toCNode :: a -> CNodelike
    
  instance Concrete Integer where
    type CNodelike = CConst
    toCNode x = CIntConst (cInteger x) internalNode
    
