{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Language.C.Miscellany where
  
  import Language.C.AST
  import Data.Maybe
  import Data.List (find)
  
  declarationIsTypedef :: CDeclaration -> Bool
  declarationIsTypedef (CDeclaration (SSpec STypedef : rest) _) = True
  declarationIsTypedef _ = False
  
  declarationIsComposite :: CDeclaration -> Bool
  declarationIsComposite (CDeclaration (TSpec (TStructOrUnion _ _ _ _) : rest) _) = True
  declarationIsComposite _ = False
  
  declarationIsFunctionPrototype :: CDeclaration -> Bool
  declarationIsFunctionPrototype (CDeclaration _ (DeclInfo { contents = Just (CDeclarator (Just _) derived _ _), ..} : _)) = 
    isJust $ find isFunction derived where
      isFunction (Function _ _) = True
      isFunction _ = False
  
  nameOfDeclaration :: CDeclaration -> Maybe String
  nameOfDeclaration (CDeclaration _ [(DeclInfo {contents, ..})]) = contents >>= nameOfDeclarator
  nameOfDeclaration _ = Nothing
  
  dropTypedef :: CDeclaration -> CDeclaration
  dropTypedef (CDeclaration (SSpec STypedef : rest) it) = CDeclaration rest it
  
  nameOfDeclarator :: CDeclarator -> Maybe String
  nameOfDeclarator (CDeclarator s _ _ _) = s
  
  derivedPartsOfDeclarator :: CDeclarator -> [DerivedDeclarator]
  derivedPartsOfDeclarator (CDeclarator _ ds _ _) = ds
  
  -- this is buggy
  isFunctionVariadic :: CFunction -> Bool
  isFunctionVariadic (CFunction _ (CDeclarator _ (Function _ b : _) _ _) _) = b
  
  -- There is probably a better way to do this with Data.Generics or something
  partitionSpecifiers :: [Specifier] -> ([TypeSpecifier], [TypeQualifier], [StorageSpecifier])
  partitionSpecifiers specs = extract ([], [], []) specs where
    extract (t, q, s) [] = (r t, r q, r s) where r = reverse
    extract (as, bs, cs) (TSpec t : rest) = extract (t:as, bs, cs) rest
    extract (as, bs, cs) (TQual q : rest) = extract (as, q:bs, cs) rest
    extract (as, bs, cs) (SSpec s : rest) = extract (as, bs, s:cs) rest