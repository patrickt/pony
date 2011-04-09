{-# LANGUAGE OverloadedStrings #-}

module Language.Pony.Transformations.Predefined.PreciseGC where
  
  import Language.Pony.Transformations.Utilities
  import Semantics.C
  import Data.Generics
  import Data.List
  import Control.Applicative
  
  refList :: SType
  refList = struct "ref_list_s" 
    [ field "parent" (pointerTo forwardRefList)
    , field "nptrs" signedInt
    , field "ref_lists" (sizedArray (pointerTo listPointer) 0)
    ]
  
  forwardRefList :: SType
  forwardRefList = emptyStruct "ref_list_s"
  
  oldList :: SType
  oldList = pointerTo $ typedef "list_t" $ struct "list_s" 
    [ field "next" $ pointerTo $ emptyStruct "list_s"
    , field "tag" tagType
    , field "c" (emptyUnion "ldata")
    ]
  
  tagType :: SType
  tagType = typedef "tag_t" $ enum ["LIST", "VALUE"]
  
  newList :: SType
  newList = struct "list_s" 
    [ field "next" $ pointerTo $ emptyStruct "list_s"
    , field "tag" $ tagType
    , field "marked" signedInt
    , field "c" $ emptyUnion "ldata"
    ]
  
  listPointer :: SType
  listPointer = pointerTo $ typedef "list_t" newList
  
  insertGCList :: [SGlobal] -> [SGlobal]
  insertGCList x = 
    let 
      gcList = globalVar "all_lists" listPointer sNull
      refListDeclaration = GComposite info where (SComposite info _) = refList
      refListInstance = globalVar "referenced_list" (pointerTo forwardRefList) sNull
      convert nil@(GVariable (Variable "nil" _ _)) = [nil, gcList, refListDeclaration, refListInstance]
      convert x = [x]
    in 
      if gcList `elem` x then x else concatMap convert x
      
  referencedListCount :: SFunction -> Int
  referencedListCount (SFunction _ _ _ params locals _) 
    = countWhere isListParameter params + countWhere isLocalList locals where
      isListParameter (SParameter _ t) = t == oldList
      isLocalList (LDeclaration (Variable _ t _)) = t == oldList
      isLocalList _ = False
    
  redefineList :: SGlobal -> SGlobal
  redefineList (GTypedef "list_t" _) = GTypedef "list_t" newList
  redefineList x = x
    
  addGC :: SFunction -> SFunction
  addGC f@(SFunction attrs typ name params locals isVariadic)
    | name == "__sputc" = f
    | name == "main" = f
    | name == "del" = removeDelete f
    | otherwise = 
    SFunction attrs typ name params (pcount : reflist : a1 : ((init locals) ++ [reset] ++ [last locals])) isVariadic where
      pcount = LDeclaration $ Variable "parameter_count" signedInt (Just <$> intToLiteral $ referencedListCount f)
      reflist = LDeclaration $ Variable "rl" forwardRefList (Nothing)
      a1 = LStatement $ ExpressionS $ Binary (Binary "rl" "." "parent") "=" "referenced_list"
      reset = LStatement $ ExpressionS $ Binary "referenced_list" "=" (Binary "rl" "." "parent")  
  
  rewriteConsOperator :: Expression -> Expression
  rewriteConsOperator (Binary lhs "::" rhs) = FunctionCall "cons" [SCast (pointerTo void) lhs, rhs]
  rewriteConsOperator x = x
  
  -- make `del` a no-op
  removeDelete :: SFunction -> SFunction
  removeDelete (SFunction attrs typ "del" params _ isVariadic) = 
    SFunction attrs typ "del" params [] isVariadic
  precise x = x
  
  gcT :: GenericT
  gcT = mkT redefineList `extT` insertGCList `extT` addGC `extT` rewriteConsOperator
  