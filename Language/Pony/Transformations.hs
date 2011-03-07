{-# LANGUAGE Rank2Types #-}

module Language.Pony.Transformations where
  
  import Data.Generics
  import qualified Language.Pony.Transformations.Predefined.CheckMalloc as CM
  import qualified Language.Pony.Transformations.Predefined.LogicalShift as LS
  import qualified Language.Pony.Transformations.Predefined.SeparateDeclarations as SD
  import qualified Language.Pony.Transformations.Predefined.HelloWorld as HW
  import qualified Language.Pony.Transformations.Predefined.StringConcat as SC
  
  data Transformation = MkTrans String GenericT
  instance Show Transformation where
    show (MkTrans s _) = s ++ "Trans"
  
  checkMalloc :: Transformation
  checkMalloc = MkTrans "CheckMalloc" CM.checkMalloc
  
  logicalShift :: Transformation
  logicalShift = MkTrans "LogicalShift" LS.logicalShift
  
  separateDeclarations :: Transformation
  separateDeclarations = MkTrans "SeparateDeclarations" SD.separateT
  
  helloWorld :: Transformation
  helloWorld = MkTrans "HelloWorld" HW.helloT
  
  stringConcat :: Transformation
  stringConcat = MkTrans "StringConcat" SC.concatT
  
  instance Read Transformation where
    readsPrec _ s = case s of
      "CheckMalloc" -> [(checkMalloc, "")]
      "LogicalShift" -> [(logicalShift, "")]
      "SeparateDeclarations" -> [(separateDeclarations, "")]
      "HelloWorld" -> [(helloWorld, "")]
      "StringConcat" -> [(stringConcat, "")]
      other -> []