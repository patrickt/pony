{-# LANGUAGE OverloadedStrings #-}

module Language.Pony.Transformations.Utilities where
  
  import Control.Applicative
  import Control.Monad
  import Data.List
  import Data.Maybe
  import GHC.Exts ( IsString(..) )
  import Semantics.C
  
  instance IsString Expression where fromString = Ident
  
  countWhere :: (a -> Bool) -> [a] -> Int
  countWhere pred x = length $ filter pred x
  
  -- | The post-processed representation of NULL
  sNull :: Expression
  sNull = Cast (SPointerTo (SVoid []) []) (intToLiteral 0)
  
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
  globalVar n t v = GVariable var [var]
    where
      var = (Variable n t (Just v))  
  
  (.=.) :: Expression -> Expression -> Expression
  a .=. b = Binary a "=" b
  
  (.->.) :: Expression -> Expression -> Expression
  a .->. b = Binary a "->" b
  
  stmt :: Expression -> Local
  stmt = (flip LStatement []) . ExpressionS 
  
  namesInGlobalScope :: Program -> [Name]
  namesInGlobalScope p = nub $ p >>= nameOf where
    nameOf (GFunction (Function _ _ n _ _ _) _) = [n]
    nameOf (GVariable (Variable n _ _) _) = [n]
    nameOf (GFunctionPrototype _ n _ _ _) = [n]
    nameOf _ = []
  
  namesInLocalScope :: Program -> Function -> [Name]
  namesInLocalScope p (Function _ _ n pms ls _) =  nub (namesInGlobalScope p ++ catMaybes (paramNames <$> pms) ++ (localNames =<< ls)) where
    paramNames (Parameter n _) = n
    localNames (LDeclaration (Variable n _ _) _) = [n]
    localNames _ = []
    
  makeHygenicName :: Name -> Program -> Function -> Name
  makeHygenicName n p f 
    = if n `elem` namesInLocalScope p f
      then makeHygenicName ("__pony" ++ n) p f
      else n
  
  enum :: [String] -> SType
  enum vals = SEnum info [] where
    info = EnumerationInfo Nothing $ toCEnum <$> vals
    toCEnum s = Enumeration s Nothing
    
  --Takes in an ASG and "fills" it with the bound variables at any local instruction or global instruction
  fillASG :: Program -> Program
  fillASG = flip fillASG' []
    
  fillASG' :: Program -> BoundVariables -> Program
  fillASG' (i:is) b = (fst filled) : fillASG'' is (b ++ (snd filled))
  fillASG [] = []
    where
      filled = fill i b
  
  fill :: SGlobal -> BoundVariables -> (SGlobal, BoundVariables)
  fill (GVariable var _) b = ((GVariable var bound), bound)
    where 
      bound = b ++ [var]
  fill i@(GFunctionPrototype t n ps bool _) b = ((GFunctionPrototype t n ps bool bound), bound)
    where
      bound = b ++ [var]
      var   = Variable n t Nothing
      
  fill (GFunction (Function as t n ps ls bool _)) b = ((Function as t n ps (fillLocals ls bound) bool ), bound)
    where
      bound = b ++ [var] ++ ps
      var = Variable n t Nothing

  fill (GTypedef n t _) b = ((GTypedef n t b), b)
  fill (GEnumeration ei _) b = ((GEnumeration ei b), b)
  fill (GComposite ci _) b = ((GComposite ci b), b)
  
  fillLocals :: [Local] -> BoundVariables -> [Local]
  fillLocals (i : is) b = (fist filled : fillLocals is (b ++ snd filled))
    where
      filled = fill' i b