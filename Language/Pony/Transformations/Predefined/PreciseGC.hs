{-# LANGUAGE OverloadedStrings #-}

module Language.Pony.Transformations.Predefined.PreciseGC where
  
  import GHC.Exts ( IsString(..) )
  import Semantics.C
  import Data.Generics
  import Data.List
  import Control.Applicative
  
  instance IsString Expression where fromString = Ident
  
  countWhere :: (a -> Bool) -> [a] -> Int
  countWhere pred x = length $ filter pred x
  
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
      gc_list = GVariable (Variable "all_lists" list_type (Just sNull))
      free_list = GVariable (Variable "marked" (SPointerTo signedInt []) (Just sNull))
      convert nil@(GVariable (Variable "nil" _ _)) = [nil, gc_list, free_list]
      convert x = [x]
    in 
      if (elem gc_list x) then x else concatMap convert x
      
  referencedListCount :: SFunction -> Int
  referencedListCount (SFunction _ _ _ params locals _) = countParams params + countLocals locals where
    countParams p = countWhere isListParameter p
    isListParameter (SParameter _ t) = t == list_type
    countLocals _ = 0
    
  addParameterCount :: SFunction -> SFunction
  addParameterCount f@(SFunction attrs typ name params locals isVariadic) = 
    SFunction attrs typ name params (pcount : locals) isVariadic where
      pcount = LDeclaration $ Variable "parameter_count" signedInt (Just <$> intToLiteral $ referencedListCount f)
  
  modifyMain :: SFunction -> SFunction
  modifyMain (SFunction attrs typ "main" params locals isVariadic) = 
    SFunction attrs typ "main" params (start ++ (init locals) ++ end) isVariadic where
      start = [ LStatement $ ExpressionS (Binary "marked" "=" (FunctionCall "malloc" [Binary (intToLiteral 35565) "*" (SizeOfSType signedInt)])) ] 
      end = [ LStatement $ Return $ Just $ intToLiteral 0 ]
  modifyMain x = x
  
  -- make `del` a no-op
  precise :: SFunction -> SFunction
  precise (SFunction attrs typ "del" params _ isVariadic) = 
    SFunction attrs typ "del" params [] isVariadic
  precise x = x
  
  gcT :: GenericT
  gcT = mkT precise `extT` insertGCList `extT` modifyMain `extT` addParameterCount