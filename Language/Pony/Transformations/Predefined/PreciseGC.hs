module Language.Pony.Transformations.Predefined.PreciseGC where
  
  import Semantics.C
  import Data.Generics
  
  -- make `del` a no-op
  precise :: SFunction -> SFunction
  precise (SFunction attrs typ "del" params _ isVariadic) = 
    SFunction attrs typ "del" params [] isVariadic
  precise x = x
  
  gcT :: GenericT
  gcT = mkT precise