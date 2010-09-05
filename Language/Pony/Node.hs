{-# LANGUAGE UnicodeSyntax #-}

module Language.Pony.Node where
  
  import Data.Data
  import Data.Dynamic
  import Data.Maybe
  import Language.C.Data.Node
  import Language.C.Pretty
  import Language.C.Syntax.AST
  import Text.PrettyPrint.HughesPJ
  
  class PNode a where
    -- Pony nodes may be convertible directly into C nodes.
    toCNode :: (CNode c) => a -> Maybe c
  
  instance (CNode a) => (PNode a) where toCNode = id