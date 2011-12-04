{-# LANGUAGE Rank2Types #-}

module Language.Pony.Transformations where
  
  import Data.Generics
  
  data Direction = TopDown | BottomUp
  
  data Transformation = MkTrans String Direction GenericT
  
  instance Show Transformation where
    show (MkTrans s _ _) = s ++ "Trans"