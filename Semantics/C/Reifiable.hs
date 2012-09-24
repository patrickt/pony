{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, RankNTypes #-}

module Semantics.C.Reifiable
 ( Reifiable (..) )
 where
  
  import Data.Generics
  import Semantics.C.ASG
  
  class (Data abstract) => Reifiable abstract where
    convert :: abstract -> Fix Sem
  