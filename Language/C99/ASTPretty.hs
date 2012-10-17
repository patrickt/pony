{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module Language.C99.ASTPretty 
  ( CLiteral (..)
  , CAsmArgument (..)
  , CAsmOperand  (..)
  , CAttribute (..)
  , CBuiltinExpr (..)
  , CDeclaration (..)
  , CDeclarator (..)
  , CDeclInfo (..)
  , CDerivedDeclarator (..)
  , CDesignator (..)
  , CEnumerator (..)
  , CExpr (..)
  , CExternal (..)
  , CField (..)
  , CFunction (..)
  , CInitializer (..)
  , CInitializerSubfield (..)
  , CParameter (..)
  , CSpecifier (..)
  , CStatement (..)
  , CStorageSpecifier (..)
  , CTypeName (..)
  , CTypeQualifier (..)
  , CTypeSpecifier (..)
  , CStringLiteral (..)
  )
  where
  
  import Language.C99.AST
  import Language.C99.Literals
  import Text.PrettyPrint.GenericPretty
  
  -- | This uses GHC's new Generics system to pretty-print AST results. 
  -- | For now, it is somewhat bewildering, as the graphs produced are gigantic,
  -- | but it's a sight better than deciphering them from a single stream of text.
  
  instance Out CLiteral
  instance Out CTranslationUnit
  instance Out CAsmArgument
  instance Out CAsmName 
  instance Out CAsmOperand 
  instance Out CAttribute
  instance Out CBlockItem
  instance Out CBuiltinExpr
  instance Out CDeclaration
  instance Out CDeclarator
  instance Out CDeclInfo
  instance Out CDerivedDeclarator
  instance Out CDesignator
  instance Out CEnumerator
  instance Out CExpr
  instance Out CExternal
  instance Out CField
  instance Out CFunction
  instance Out CInitializer
  instance Out CInitializerSubfield
  instance Out CParameter
  instance Out CSpecifier
  instance Out CStatement
  instance Out CStorageSpecifier
  instance Out CTypeName
  instance Out CTypeQualifier
  instance Out CTypeSpecifier
  instance Out CStringLiteral