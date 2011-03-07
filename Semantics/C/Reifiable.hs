{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Semantics.C.Reifiable
 ( Reifiable (..) )
 where
  
  class Reifiable abstract semantic | abstract -> semantic where
    convert :: abstract -> semantic
  