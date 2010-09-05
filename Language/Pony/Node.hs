{-# LANGUAGE UnicodeSyntax, TypeFamilies, GADTs #-}

module Language.Pony.Node where
  
  import Control.Applicative
  import Language.C.Data.Node
  import Language.C.Syntax.AST
  import Language.C.Syntax.Constants
    
  -- The Concrete typeclass represents things that can be compiled down to C expressions.
  -- At the end of all of the transformations, every node in the syntax tree must 
  -- be a Concrete node. New functions and builtins can be provided by implementing new 
  -- instances of Concrete.
  class Concrete a where
    toExpr :: a -> CExpr
  
  -- All C expressions are representable in a concrete manner, obviously.
  instance Concrete CExpr where toExpr = id
  
  -- But so are all Haskell Ints.
  instance Concrete Int where
    toExpr i = CConst $ CIntConst boosted internalNode where boosted = cInteger . toInteger $ i
    
    
  