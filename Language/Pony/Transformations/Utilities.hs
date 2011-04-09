{-# LANGUAGE OverloadedStrings #-}

module Language.Pony.Transformations.Utilities where
  
  import Control.Applicative
  import GHC.Exts ( IsString(..) )
  import Semantics.C
  
  instance IsString Expression where fromString = Ident
  
  countWhere :: (a -> Bool) -> [a] -> Int
  countWhere pred x = length $ filter pred x
  
  -- | The post-processed representation of NULL
  sNull :: Expression
  sNull = SCast (SPointerTo (SVoid []) []) (intToLiteral 0)
  
  emptyStruct :: String -> SType
  emptyStruct s = SComposite (CompositeInfo Struct (Just s) []) []
  
  emptyUnion :: String -> SType
  emptyUnion s = SComposite (CompositeInfo Union (Just s) []) []
  
  struct :: String -> [SField] -> SType
  struct n fs = SComposite (CompositeInfo Struct (Just n) fs) []
  
  field :: String -> SType -> SField
  field n t = SField (Just n) t Nothing
  
  pointerTo :: SType -> SType
  pointerTo t = SPointerTo t []
  
  sizedArray :: SType -> Int -> SType
  sizedArray t z = SArray t (Just (intToLiteral z)) []
  
  typedef :: String -> SType -> SType
  typedef n t = Typedef n t []
  
  globalVar :: String -> SType -> Expression -> SGlobal
  globalVar n t v = GVariable (Variable n t (Just v))
  
  (.=.) :: Expression -> Expression -> Expression
  a .=. b = Binary a "=" b
  
  enum :: [String] -> SType
  enum vals = SEnum info [] where
    info = EnumerationInfo Nothing $ toCEnum <$> vals
    toCEnum s = Enumeration s Nothing