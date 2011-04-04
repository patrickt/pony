module Language.Pony.Transformations.Predefined.PreciseGC where
  
  import Semantics.C
  import Data.Generics
  import Data.List
  
  -- Avert your gaze!
  list_type = (SPointerTo (Typedef "list_t" (SComposite (CompositeInfo 
    Struct (Just "list_s") [SField (Just "next") (SPointerTo 
    (SComposite (CompositeInfo Struct (Just "list_s") []) []) []) 
    Nothing,SField (Just "tag") (Typedef "tag_t" (SEnum (EnumerationInfo Nothing 
    [Enumeration "LIST" Nothing,Enumeration "VALUE" Nothing]) []) []) 
    Nothing,SField (Just "c") (SComposite (CompositeInfo Union 
    (Just "ldata") []) []) Nothing]) []) []) [])
  
  -- the post-processed representation of NULL.
  -- I don't like it either.
  sNull = SCast (SPointerTo (SVoid []) []) (intToLiteral 0)
  
  -- Even if I wasn't headed to hell, this would get me an assigned seat.
  insertGCList :: [SGlobal] -> [SGlobal]
  insertGCList x = 
    let 
      gc_list = GVariable (Variable "gc_list" list_type (Just sNull))
      free_list = GVariable (Variable "free_list" list_type (Just sNull))
      convert nil@(GVariable (Variable "nil" _ _)) = [nil, gc_list, free_list]
      convert x = [x]
    in 
      if (elem gc_list x) then x else concatMap convert x
  
  -- make `del` a no-op
  precise :: SFunction -> SFunction
  precise (SFunction attrs typ "del" params _ isVariadic) = 
    SFunction attrs typ "del" params [] isVariadic
  precise x = x
  
  gcT :: GenericT
  gcT = mkT precise `extT` insertGCList