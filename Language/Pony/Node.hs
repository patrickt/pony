{-# LANGUAGE UnicodeSyntax, Rank2Types, DeriveDataTypeable, StandaloneDeriving, GeneralizedNewtypeDeriving, ExistentialQuantification #-}

module Language.Pony.Node where
  
  import Control.Applicative
  import Data.Functor
  import Data.Generics
  import Language.C.Parser
  import Language.C.Syntax.AST
  import Language.C.Data.Node
  import Language.FiletOFish.Constructs hiding (Data)
  import Language.FiletOFish.PureExpressions hiding (cast)
  import Language.FiletOFish.Semantics
  
  data Info = Info NodeInfo deriving (Typeable)
  