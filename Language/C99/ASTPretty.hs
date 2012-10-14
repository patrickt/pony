{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, StandaloneDeriving, OverlappingInstances #-}

module Language.C99.ASTPretty 
  where
  
  import Data.Generics
  import Language.C99.AST
  import Language.C99.Literals
  import Text.PrettyPrint.GenericPretty
  
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