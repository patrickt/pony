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
  declarationIsComposite (CDeclaration specs _) = isJust $ find isComposite specs where
    isComposite (TSpec (TStructOrUnion _ _ _ _)) = True
    isComposite _ = False
  
  -- A declaration is a function or function prototype if its derived declarations
  declarationIsFunctionPrototype :: CDeclaration -> Bool
  declarationIsFunctionPrototype (CDeclaration _ (DeclInfo { contents = Just (CDeclarator (Just _) derived _ _), ..} : _)) = 
    isJust $ find isFunction derived where
      isFunction (Function _ _) = True
      isFunction _ = False
  declarationIsFunctionPrototype _ = False
  
  declarationHasPointer :: CDeclaration -> Bool
  declarationHasPointer (CDeclaration _ infos) = isJust $ find hasPointer infos where
    hasPointer :: DeclInfo -> Bool
    hasPointer (DeclInfo {contents = Just decl, ..}) = declaratorIsPointer decl
    
  declaratorIsPointer :: CDeclarator -> Bool
  declaratorIsPointer (CDeclarator _ derived _ _) = isJust $ find isPointer derived where
    isPointer :: DerivedDeclarator -> Bool
    isPointer (Pointer _) = True
    isPointer _ = False
  
  nameOfDeclaration :: CDeclaration -> Maybe String
  nameOfDeclaration (CDeclaration _ [DeclInfo {contents, ..}]) = contents >>= nameOfDeclarator
  nameOfDeclaration _ = Nothing
  
  dropTypedef :: CDeclaration -> CDeclaration
  dropTypedef (CDeclaration (SSpec STypedef : rest) it) = CDeclaration rest it
  
  nameOfDeclarator :: CDeclarator -> Maybe String
  nameOfDeclarator (CDeclarator s _ _ _) = s
  
  derivedPartsOfDeclarator :: CDeclarator -> [DerivedDeclarator]
  derivedPartsOfDeclarator (CDeclarator _ ds _ _) = ds
  
  doesDeclaratorContainVariadicSpecifier :: CDeclarator -> Bool
  doesDeclaratorContainVariadicSpecifier d = isJust $ find variadicFunction (derivedPartsOfDeclarator d) where
    variadicFunction (Function _ True) = True
    variadicFunction _ = False
  
  -- this is buggy
  isFunctionVariadic :: CFunction -> Bool
  isFunctionVariadic (CFunction _ d _) = doesDeclaratorContainVariadicSpecifier d
  
  -- There is probably a better way to do this with Data.Generics or something
  partitionSpecifiers :: [Specifier] -> ([TypeSpecifier], [TypeQualifier], [StorageSpecifier])
  partitionSpecifiers specs = extract ([], [], []) specs where
    extract (t, q, s) [] = (r t, r q, r s) where r = reverse
    extract (as, bs, cs) (TSpec t : rest) = extract (t:as, bs, cs) rest
    extract (as, bs, cs) (TQual q : rest) = extract (as, q:bs, cs) rest
    extract (as, bs, cs) (SSpec s : rest) = extract (as, bs, s:cs) rest