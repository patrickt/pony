{-# LANGUAGE Rank2Types #-}

module Language.Pony.Transformation where
  
  import Data.Generics
  import qualified Language.Pony.CheckMalloc as CM
  import qualified Language.Pony.LogicalShift as LS
  import qualified Language.Pony.SeparateDeclarations as SD
  
  data Transformation = MkTrans String GenericT
  instance Show Transformation where
    show (MkTrans s _) = s ++ "Trans"
  
  checkMalloc :: Transformation
  checkMalloc = MkTrans "CheckMalloc" CM.checkMalloc
  
  logicalShift :: Transformation
  logicalShift = MkTrans "LogicalShift" LS.logicalShift
  
  separateDeclarations :: Transformation
  separateDeclarations = MkTrans "SeparateDeclarations" SD.separateT
  
  instance Read Transformation where
    readsPrec _ s = case s of
      "CheckMalloc" -> [(checkMalloc, "")]
      "LogicalShift" -> [(logicalShift, "")]
      "SeparateDeclarations" -> [(separateDeclarations, "")]
      other -> []