{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, RankNTypes #-}

module Semantics.C.Reifiable
 ( Reifiable (..) )
 where
  
  import Semantics.C.ASG
  import Data.Functor.Fix
  
  -- coalgebra?
  class Reifiable abstract where
    convert :: abstract -> Fix Sem
  