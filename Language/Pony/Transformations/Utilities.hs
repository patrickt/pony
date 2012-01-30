{-# LANGUAGE OverloadedStrings #-}

module Language.Pony.Transformations.Utilities where
  
  import Control.Applicative
  import Control.Monad
  import Data.List
  import Data.Maybe
  import GHC.Exts ( IsString(..) )
  import Semantics.C
  import Data.Generics
  import Semantics.C.HasBound.Instances
  import Semantics.C.HasBound
  
  instance IsString Expression where fromString = flip Ident []
  
  countWhere :: (a -> Bool) -> [a] -> Int
  countWhere pred x = length $ filter pred x
  
  -- | The post-processed representation of NULL
  sNull :: Expression
  sNull = Cast (SPointerTo (SVoid []) []) (intToLiteral 0) []
  
  emptyStruct :: String -> SType
  emptyStruct s = SComposite (CompositeInfo Struct (Just s) []) []
  
  emptyUnion :: String -> SType
  emptyUnion s = SComposite (CompositeInfo Union (Just s) []) []
  
  struct :: String -> [Field] -> SType
  struct n fs = SComposite (CompositeInfo Struct (Just n) fs) []
  
  field :: String -> SType -> Field
  field n t = Field (Just n) t Nothing
  
  pointerTo :: SType -> SType
  pointerTo t = SPointerTo t []
  
  sizedArray :: SType -> Int -> SType
  sizedArray t z = SArray t (Just (intToLiteral z)) []
  
  typedef :: String -> SType -> SType
  typedef n t = STypedef n t []
  
  globalVar :: String -> SType -> Expression -> SGlobal
  globalVar n t v = GVariable var [n]
    where
      var = (Variable n t (Just v))
  
  (.=.) :: Expression -> Expression -> Expression
  a .=. b = Binary a "=" b (getBoundNames a ++ getBoundNames b)
  
  (.->.) :: Expression -> Expression -> Expression
  a .->. b = Binary a "->" b (getBoundNames a ++ getBoundNames b)
  
  stmt :: Expression -> Local
  stmt = (flip LStatement []) . flip ExpressionS []
  
  enum :: [String] -> SType
  enum vals = SEnum info [] where
    info = EnumerationInfo Nothing $ toCEnum <$> vals
    toCEnum s = Enumeration s Nothing
    
  --Takes in an ASG and "fills" it with the bound variables at any local instruction or global instruction
  fillASG :: Program -> Program
  fillASG = flip fillASG' []
    
  fillASG' :: Program -> BoundVariables -> Program
  fillASG' (i:is) b = 
    let
      filled = doFill i b
    in
     (fst filled) : fillASG' is (snd filled)
  fillASG' [] _ = []

  doFill :: SGlobal -> BoundVariables -> (SGlobal, BoundVariables)
  doFill full@(GFunction f@(Function as st n ps ls bl) bv') bv = (GFunction filledF new, new)
    where
      filledF = Function as st n ps (map (\x -> fst (fill  x bv)) ls) bl
      new = snd $ fill full bv
  doFill full bv = fill full bv


{-  fill :: SGlobal -> BoundVariables -> (SGlobal, BoundVariables)
  fill (GVariable var@(Variable name _ _) _) b = let bound = b ++ [name] in ((GVariable var bound), bound)
  fill (GFunctionPrototype t n ps bool _) b = let
    bound = b ++ [n]
    in
    ((GFunctionPrototype t n ps bool bound), bound)
  fill (GFunction (Function as t n ps ls bool) _) b = let
    bound = b ++ [n] ++ ps'
    ps' = concatMap (\(Parameter nm _) -> if isJust nm then [fromJust nm] else []) ps
    in    
     (GFunction (Function as t n ps (fillLocals ls bound) bool) bound, bound)
  fill (GTypedef n t _) b = ((GTypedef n t b), b)
  fill (GEnumeration ei _) b = ((GEnumeration ei b), b)
  fill (GComposite ci _) b = ((GComposite ci b), b)
  
  fillLocals :: [Local] -> BoundVariables -> [Local]
  fillLocals (i : is) b = let filled = fill' i b in (fst filled : fillLocals is (b ++ snd filled))
  fillLocals [] _ = []

  fill' :: Local -> BoundVariables -> (Local, BoundVariables)
  fill' (LStatement s _) b = ((LStatement s b), b)
  fill' (LDeclaration var@(Variable n _ _) _) b = ((LDeclaration var (b ++ [n])), b ++ [n])
-}