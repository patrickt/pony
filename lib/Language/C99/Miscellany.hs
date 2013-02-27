module Language.C99.Miscellany where
  
  
  import Language.C99.AST
  import Control.Applicative
  import Data.Monoid
  import Data.Generics.Uniplate.Data
  import Data.Maybe
  -- 
  -- any' :: [a] -> Bool
  -- any' = not . null
  -- 
  -- -- | A declaration is a typedef iff its first specifier is an 'STypedef'.
  -- declarationIsTypedef :: CDeclaration -> Bool
  -- declarationIsTypedef (CDeclaration (SSpec CTypedef : _) _) = True
  -- declarationIsTypedef _ = False
  -- 
  -- -- A declaration is of a composite type if it contains a 'TStructOrUnion' specifier.
  -- declarationIsComposite :: CDeclaration -> Bool
  -- -- declarationIsComposite decl = any' [i | i@(TStructOrUnion {}) <- universeBi decl]
  -- declarationIsComposite = undefined
  -- 
  -- -- A declaration is a function or function prototype if its derived declarations 
  -- -- contain the DerivedFunction attribute.
  -- declarationIsFunctionPrototype :: CDeclaration -> Bool
  -- declarationIsFunctionPrototype (CDeclaration _ (CDeclInfo { contents, .. } : _)) = 
  --   any isFunction (derived contents) where
  --     isFunction (DerivedFunction _ _) = True
  --     isFunction _ = False
  -- declarationIsFunctionPrototype _ = False
  -- 
  -- declarationIsEnum :: CDeclaration -> Bool
  -- -- declarationIsEnum decl = any' [i | i@(TEnumeration {}) <- universeBi decl]
  -- declarationIsEnum = undefined
  -- 
  -- declarationHasEnumerations :: CDeclaration -> Bool
  -- -- declarationHasEnumerations decl = any' [e | e@(EnumIdent {}) <- universeBi decl]
  -- declarationHasEnumerations = undefined
  -- 
  -- declarationHasFields :: CDeclaration -> Bool
  -- -- declarationHasFields d = any' [f | f@(CField _) <- universeBi d]
  -- declarationHasFields = undefined
  -- 
  -- nameOfDeclaration :: CDeclaration -> Maybe String
  -- nameOfDeclaration (CDeclaration _ [CDeclInfo {contents, ..}]) = declName contents 
  -- nameOfDeclaration _ = Nothing
  -- 
  -- declarationIsNamed :: CDeclaration -> Bool
  -- declarationIsNamed = isJust . nameOfDeclaration
  -- 
  -- declarationIsUnnamed :: CDeclaration -> Bool
  -- declarationIsUnnamed = isNothing . nameOfDeclaration
  -- 
  -- dropTypedef :: CDeclaration -> CDeclaration
  -- dropTypedef (CDeclaration (SSpec CTypedef : rest) it) = CDeclaration rest it
  -- dropTypedef _ = error "invalid declaration passed to dropTypedef"
  -- 
  -- declaratorContainsVariadicSpecifier :: CDeclarator -> Bool
  -- declaratorContainsVariadicSpecifier d = any variadicFunction (derived d) where
  --   variadicFunction (DerivedFunction _ True) = True
  --   variadicFunction _ = False
  -- 
  -- -- this is buggy
  -- isFunctionVariadic :: CFunction -> Bool
  -- isFunctionVariadic (CFunction _ d _) = declaratorContainsVariadicSpecifier d
  -- 
  -- partitionSpecifiers :: [CSpecifier] -> ([CTypeSpecifier], [CTypeQualifier], [CStorageSpecifier])
  -- partitionSpecifiers them = (typeSpecs, typeQuals, storageSpecs) where 
  --   typeQuals = [ a | (TQual a) <- them ]
  --   typeSpecs = [ a | (TSpec a) <- them ]
  --   storageSpecs = [ a | (SSpec a) <- them ]
  --   
  -- declaratorParameters :: CDeclarator -> [CParameter]
  -- declaratorParameters d = mconcat $ go <$> (derived d) where
  --   go (DerivedFunction ps _) = ps
  --   go _ = []
  -- 
