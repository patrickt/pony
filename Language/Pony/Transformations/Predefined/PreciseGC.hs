{-# LANGUAGE OverloadedStrings #-}

module Language.Pony.Transformations.Predefined.PreciseGC where
  
  import Language.Pony.Transformations.Utilities
  import Language.Pony.Transformations.Predefined.SeparateDeclarations
  import Semantics.C
  import Data.Generics
  import Data.List
  import Control.Applicative
  
  refList :: SType
  refList = struct "ref_list_s" 
    [ field "parent" (pointerTo forwardRefList)
    , field "nptrs" signedInt
    , field "ref_lists" (sizedArray listPointer 16)
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
  referencedListCount f = length $ referencedLists f 
      
  referencedLists :: SFunction -> [String]
  referencedLists (SFunction _ _ _ params locals _) =
    (paramName <$> filter isListParameter params) ++ (localName <$> filter isLocalList locals) where
      paramName (SParameter (Just n) _) = n
      localName (LDeclaration (Variable n _ _)) = n
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
    SFunction attrs typ name params (reflist : a1 : a2 : a3 :  (declarations ++ boilerplate ++ (init assignments) ++ [reset] ++ [last assignments])) isVariadic where
      (declarations, assignments) = partitionLocals locals
      toAssignment (str,n) = stmt $ Binary (Brackets (Binary "rl" "." "reflists") (intToLiteral n)) "=" (Unary "&" (Ident str))
      boilerplate = toAssignment <$> (zip (referencedLists f) [0..(referencedListCount f)])
      reflist = LDeclaration $ Variable "rl" forwardRefList Nothing
      a1 = stmt $ (Binary "rl" "." "parent") .=. "referenced_list"
      a2 = stmt $ (Binary "rl" "." "nptrs") .=. (intToLiteral $ referencedListCount f)
      a3 = stmt $ "referenced_list" .=. (Unary "&" "rl")
      reset = stmt $ "referenced_list" .=. (Binary "rl" "." "parent")  
  
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
  