{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Language.C.Miscellany where
  
  import Language.C.AST
  import Data.Maybe
  import Data.List (find)
  
  -- | A declaration is a typedef iff its first specifier is an 'STypedef'.
  declarationIsTypedef :: CDeclaration -> Bool
  declarationIsTypedef (CDeclaration (SSpec STypedef : rest) _) = True
  declarationIsTypedef _ = False
  
  -- A declaration is of a composite type if it contains a 'TStructOrUnion' specifier.
  declarationIsComposite :: CDeclaration -> Bool
  declarationIsComposite (CDeclaration specs _) = any isComposite specs where
    isComposite (TSpec (TStructOrUnion _ _ _ _)) = True
    isComposite _ = False
  
  -- A declaration is a function or function prototype if its derived declarations
  declarationIsFunctionPrototype :: CDeclaration -> Bool
  declarationIsFunctionPrototype (CDeclaration _ (DeclInfo { contents = Just (CDeclarator (Just _) derived _ _), ..} : _)) = 
    any isFunction derived where
      isFunction (Function _ _) = True
      isFunction _ = False
  declarationIsFunctionPrototype _ = False
  
  declarationHasPointer :: CDeclaration -> Bool
  declarationHasPointer (CDeclaration _ infos) = any hasPointer infos where
    hasPointer :: DeclInfo -> Bool
    hasPointer (DeclInfo {contents = Just decl, ..}) = declaratorIsPointer decl
    
  declaratorIsPointer :: CDeclarator -> Bool
  declaratorIsPointer (CDeclarator _ derived _ _) = any isPointer derived where
    isPointer :: DerivedDeclarator -> Bool
    isPointer (Pointer _) = True
    isPointer _ = False
  
  nameOfDeclaration :: CDeclaration -> Maybe String
  nameOfDeclaration (CDeclaration _ [DeclInfo {contents, ..}]) = contents >>= declName
  nameOfDeclaration _ = Nothing
  
  dropTypedef :: CDeclaration -> CDeclaration
  dropTypedef (CDeclaration (SSpec STypedef : rest) it) = CDeclaration rest it
  
  doesDeclaratorContainVariadicSpecifier :: CDeclarator -> Bool
  doesDeclaratorContainVariadicSpecifier d = any variadicFunction (derived d) where
    variadicFunction (Function _ True) = True
    variadicFunction _ = False
  
  -- this is buggy
  isFunctionVariadic :: CFunction -> Bool
  isFunctionVariadic (CFunction _ d _) = doesDeclaratorContainVariadicSpecifier d
  
  partitionSpecifiers :: [Specifier] -> ([TypeSpecifier], [TypeQualifier], [StorageSpecifier])
  partitionSpecifiers them = (typeSpecs, typeQuals, storageSpecs) where 
    typeQuals = [ a | (TQual a) <- them ]
    typeSpecs = [ a | (TSpec a) <- them ]
    storageSpecs = [ a | (SSpec a) <- them ]

  specifierBelongsToFunction :: Specifier -> Bool
  specifierBelongsToFunction (SSpec SStatic) = True
  specifierBelongsToFunction (SSpec SExtern) = True
  specifierBelongsToFunction (TQual FInline) = True
  specifierBelongsToFunction _ = False
