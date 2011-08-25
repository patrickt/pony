{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Language.C.Miscellany where
  
  import Language.C.AST
  
  -- | A declaration is a typedef iff its first specifier is an 'STypedef'.
  declarationIsTypedef :: CDeclaration -> Bool
  declarationIsTypedef (CDeclaration (SSpec CTypedef : _) _) = True
  declarationIsTypedef _ = False
  
  -- A declaration is of a composite type if it contains a 'TStructOrUnion' specifier.
  declarationIsComposite :: CDeclaration -> Bool
  declarationIsComposite (CDeclaration specs _) = any isComposite specs where
    isComposite (TSpec (TStructOrUnion _ _ _ _)) = True
    isComposite _ = False
  
  -- A declaration is a function or function prototype if its derived declarations
  declarationIsFunctionPrototype :: CDeclaration -> Bool
  declarationIsFunctionPrototype (CDeclaration _ (CDeclInfo { contents = Just (CDeclarator (Just _) derived _ _), ..} : _)) = 
    any isFunction derived where
      isFunction (DerivedFunction _ _) = True
      isFunction _ = False
  declarationIsFunctionPrototype _ = False
  
  declarationHasPointer :: CDeclaration -> Bool
  declarationHasPointer (CDeclaration _ infos) = any hasPointer infos where
    hasPointer :: CDeclInfo -> Bool
    hasPointer (CDeclInfo {contents = Just decl, ..}) = declaratorIsPointer decl
    hasPointer _ = False
    
  declaratorIsPointer :: CDeclarator -> Bool
  declaratorIsPointer (CDeclarator _ derived _ _) = any isPointer derived where
    isPointer :: CDerivedDeclarator -> Bool
    isPointer (Pointer _) = True
    isPointer _ = False
  
  nameOfDeclaration :: CDeclaration -> Maybe String
  nameOfDeclaration (CDeclaration _ [CDeclInfo {contents, ..}]) = contents >>= declName
  nameOfDeclaration _ = Nothing
  
  dropTypedef :: CDeclaration -> CDeclaration
  dropTypedef (CDeclaration (SSpec CTypedef : rest) it) = CDeclaration rest it
  dropTypedef _ = error "invalid declaration passed to dropTypedef"
  
  doesDeclaratorContainVariadicSpecifier :: CDeclarator -> Bool
  doesDeclaratorContainVariadicSpecifier d = any variadicFunction (derived d) where
    variadicFunction (DerivedFunction _ True) = True
    variadicFunction _ = False
  
  -- this is buggy
  isFunctionVariadic :: CFunction -> Bool
  isFunctionVariadic (CFunction _ d _) = doesDeclaratorContainVariadicSpecifier d
  
  partitionSpecifiers :: [CSpecifier] -> ([CTypeSpecifier], [CTypeQualifier], [CStorageSpecifier])
  partitionSpecifiers them = (typeSpecs, typeQuals, storageSpecs) where 
    typeQuals = [ a | (TQual a) <- them ]
    typeSpecs = [ a | (TSpec a) <- them ]
    storageSpecs = [ a | (SSpec a) <- them ]

  specifierBelongsToFunction :: CSpecifier -> Bool
  specifierBelongsToFunction (SSpec CStatic) = True
  specifierBelongsToFunction (SSpec CExtern) = True
  specifierBelongsToFunction (TQual CInline) = True
  specifierBelongsToFunction _ = False
