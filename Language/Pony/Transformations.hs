{-# LANGUAGE Rank2Types #-}

module Language.Pony.Transformations where
  
  import Data.Generics
  import qualified Language.Pony.Transformations.Predefined.CheckMalloc as CM
  import qualified Language.Pony.Transformations.Predefined.LogicalShift as LS
  import qualified Language.Pony.Transformations.Predefined.SeparateDeclarations as SD
  import qualified Language.Pony.Transformations.Predefined.HelloWorld as HW
  import qualified Language.Pony.Transformations.Predefined.StringConcat as SC
  import qualified Language.Pony.Transformations.Predefined.PreciseGC as PGC
  
  data Direction = TopDown | BottomUp
  
  data Transformation = MkTrans String Direction GenericT
  
  instance Show Transformation where
    show (MkTrans s _ _) = s ++ "Trans"
  
  checkMalloc :: Transformation
  checkMalloc = MkTrans "CheckMalloc" TopDown CM.checkMalloc
  
  logicalShift :: Transformation
  logicalShift = MkTrans "LogicalShift" TopDown LS.logicalShift
  
  separateDeclarations :: Transformation
  separateDeclarations = MkTrans "SeparateDeclarations" TopDown SD.separateT
  
  helloWorld :: Transformation
  helloWorld = MkTrans "HelloWorld" TopDown HW.helloT
  
  stringConcat :: Transformation
  stringConcat = MkTrans "StringConcat" TopDown SC.concatT
  
  preciseGC :: Transformation
  preciseGC = MkTrans "PreciseGC" TopDown PGC.gcT
  
  instance Read Transformation where
    readsPrec _ s = case s of
      "CheckMalloc" -> [(checkMalloc, "")]
      "LogicalShift" -> [(logicalShift, "")]
      "SeparateDeclarations" -> [(separateDeclarations, "")]
      "HelloWorld" -> [(helloWorld, "")]
      "StringConcat" -> [(stringConcat, "")]
      "PreciseGC" -> [(preciseGC, "")]
      other -> []