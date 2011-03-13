{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Semantics.C.Reifiable
 ( Reifiable (..) )
 where
  
  import Data.Generics
  
  class (Data semantic) => Reifiable abstract semantic | abstract -> semantic where
    convert :: abstract -> semantic
  