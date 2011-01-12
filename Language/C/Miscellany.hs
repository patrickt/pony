module Language.C.Miscellany where
  
  import Language.C.AST
  
  nameOfDeclarator :: CDeclarator -> Maybe String
  nameOfDeclarator (Named s _ _ _) = Just s
  nameOfDeclarator (Abstract _ _) = Nothing
  
  derivedPartsOfDeclarator :: CDeclarator -> [DerivedDeclarator]
  derivedPartsOfDeclarator (Named _ ds _ _) = ds
  derivedPartsOfDeclarator (Abstract ds _) = ds