{-# LANGUAGE UnicodeSyntax, Rank2Types, MultiParamTypeClasses, StandaloneDeriving #-}

module Language.Pony.Node where

  import Data.Tree
  import Data.Data
  import Language.C.Data.Node
  import Language.C.Syntax.AST
  import Language.C.Syntax.Constants
  
  cNodeToTree :: (Data a) => a -> Tree Constr
  cNodeToTree x = Node { rootLabel = toConstr x, subForest = cNodeToTree `gmapQ` x }
  
  deriving instance Show CTranslUnit
  deriving instance Show CExtDecl
  deriving instance Show CStrLit
  deriving instance Show CFunDef
  deriving instance Show CDecl
  deriving instance Show CInit
  deriving instance Show CExpr
  deriving instance Show CBuiltin
  deriving instance Show CConst
  deriving instance Show CDesignator
  deriving instance Show CStat
  deriving instance Show CDeclr
  deriving instance Show CDeclSpec
  deriving instance Show CTypeQual
  deriving instance Show CTypeSpec
  deriving instance Show CEnum
  deriving instance Show CStructUnion
  deriving instance Show CStructTag
  deriving instance Show CDerivedDeclr
  deriving instance Show CArrSize
  deriving instance Show CAsmStmt
  deriving instance Show CBlockItem
  deriving instance Show CAttr
  deriving instance Show CAsmOperand
