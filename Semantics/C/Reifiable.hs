{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, RankNTypes #-}

module Semantics.C.Reifiable
 ( Reifiable (..) )
 where
  
  import Semantics.C.ASG
  import Data.Generics.Fixplate
  
  -- coalgebra?
  class Reifiable abstract where
    convert :: abstract -> Mu Sem
  