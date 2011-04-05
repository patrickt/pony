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
  
  ref_list_type = (CompositeInfo Struct (Just "ref_list_s") 
    [SField (Just "parent") (SPointerTo (SComposite forward_ref_list_type []) []) Nothing])
  
  forward_ref_list_type = (CompositeInfo Struct (Just "ref_list_s") [])
  
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
      ref_list_decl = GComposite $ ref_list_type
      ref_list_instance = GVariable (Variable "referenced_list" (SPointerTo (SComposite forward_ref_list_type []) []) (Just sNull))
      marked = GVariable (Variable "marked" (SPointerTo signedInt []) (Just sNull))
      convert nil@(GVariable (Variable "nil" _ _)) = [nil, gc_list, marked, ref_list_decl, ref_list_instance]
      convert x = [x]
    in 
      if (elem gc_list x) then x else concatMap convert x
      
  referencedListCount :: SFunction -> Int
  referencedListCount (SFunction _ _ _ params locals _) = countParams params + countLocals locals where
    countParams = countWhere isListParameter
    isListParameter (SParameter _ t) = t == list_type
    countLocals = countWhere isLocalList
    isLocalList (LDeclaration (Variable _ t _)) = t == list_type 
    isLocalList _ = False
    
  addGC :: SFunction -> SFunction
  addGC f@(SFunction attrs typ name params locals isVariadic)
    | name == "__sputc" = f
    | name == "main" = f
    | otherwise = 
    SFunction attrs typ name params (pcount : reflist : a1 : ((init locals) ++ [reset] ++ [last locals])) isVariadic where
      pcount = LDeclaration $ Variable "parameter_count" signedInt (Just <$> intToLiteral $ referencedListCount f)
      reflist = LDeclaration $ Variable "rl" (SComposite forward_ref_list_type []) (Nothing)
      a1 = LStatement $ ExpressionS $ Binary (Binary "rl" "." "parent") "=" "referenced_list"
      reset = LStatement $ ExpressionS $ Binary "referenced_list" "=" (Binary "rl" "." "parent")
      
      
  
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
  gcT = mkT precise `extT` insertGCList `extT` modifyMain `extT` addGC