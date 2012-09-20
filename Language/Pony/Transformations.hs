{-# LANGUAGE Rank2Types #-}

module Language.Pony.Transformations where
  
  import Data.Generics
  import Generics.Regular.Rewriting
  import Semantics.C.ASG
  
  data Direction = TopDown | BottomUp
  
  data Transformation 
    = MkTrans String Direction GenericT
    | RewriteTrans String (Rule Program)
  
  instance Show Transformation where
    show (MkTrans s _ _) = s ++ "Trans"