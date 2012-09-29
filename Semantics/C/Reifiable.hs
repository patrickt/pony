{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, RankNTypes #-}

module Semantics.C.Reifiable
 ( Reifiable (..) )
 where
  
  import Data.Generics
  import Semantics.C.ASG
  
  class Reifiable abstract where
    convert :: abstract -> Fix Sem
  