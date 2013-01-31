module Semantics.C.ASG.Newtypes where 

  import Semantics.C.ASG

  newtype FixSignedness = FSign { unFSign :: FSem } deriving (Show)
  newtype FixSize = FSize { unFSize :: FSem } deriving (Show)
  newtype FixType = FType { unFType :: FSem } deriving (Show)
  newtype FixNonVoidType = FNVType { unFNVType :: FSem } deriving (Show)
  newtype FixName = FName { unFName :: FSem } deriving (Show)
  newtype FixVar = FVar { unFVar :: FSem } deriving (Show)
  newtype FixVarAssign = FVarAssign { unFVarAssign :: FSem } deriving (Show)