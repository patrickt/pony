module Language.C.Miscellany where
  
  import Language.C.AST
  
  fst3 :: (a, b, c) -> a
  fst3 (a, _, _) = a
  
  nameOfDeclarator :: CDeclarator -> Maybe String
  nameOfDeclarator (CDeclarator s _ _ _) = s
  
  derivedPartsOfDeclarator :: CDeclarator -> [DerivedDeclarator]
  derivedPartsOfDeclarator (CDeclarator _ ds _ _) = ds
  
  isFunctionVariadic :: CFunction -> Bool
  isFunctionVariadic (CFunction _ (CDeclarator _ ((Function _ b):rest) _ _) _) = b
  
  -- There is probably a better way to do this with Data.Generics or something
  partitionSpecifiers :: [Specifier] -> ([TypeSpecifier], [TypeQualifier], [StorageSpecifier])
  partitionSpecifiers specs = extract ([], [], []) specs where
    extract (t, q, s) [] = (r t, r q, r s) where r = reverse
    extract (as, bs, cs) (TSpec t : rest) = extract (t:as, bs, cs) rest
    extract (as, bs, cs) (TQual q : rest) = extract (as, q:bs, cs) rest
    extract (as, bs, cs) (SSpec s : rest) = extract (as, bs, s:cs) rest