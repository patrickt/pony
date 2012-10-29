{-# LANGUAGE NamedFieldPuns, OverlappingInstances, ViewPatterns #-}

module Language.Pony 
  ( module Semantics.C.ASG
  , module Semantics.C.Pretty
  , module Semantics.C.Reifiable
  , module Language.C99
  , module Data.Functor.Fix
  ) 
  
  where
    -- 
  import Data.Functor.Fix
  import Language.C99 hiding (CChar, CFloat, Empty, parse, attribute, typedefs)
  import Data.Generics.Fixplate.Attributes
  import Data.Generics.Fixplate.Draw
  import Data.Generics.Fixplate.Traversals
  import Semantics.C.ASG
  import Semantics.C.Reifiable
  import Semantics.C.Reifiable.Instances
  import Semantics.C.Pretty
  import Text.PrettyPrint.GenericPretty
  import qualified Data.Foldable as F
  import qualified Data.List as L
  import Semantics.C.Queries
  import Semantics.C.ASG.Newtypes
  import Semantics.C.ASG.Arbitrary
  import Test.QuickCheck
  import Debug.Trace
  
  type NSem = Mu (Ann Sem Int)
  
  churn :: [String] -> NSem -> NSem
  churn [] x = x
  churn (name:rest) x = churn rest $ transform (killTypedefs name (attribute $ firstTypedef name x)) x
  
  reifyTypedefs :: FSem -> FSem
  reifyTypedefs x = forget (churn (typedefNames (enumerateNodes_ x)) $ enumerateNodes_ x)
  
  killTypedefs :: String -> Int -> NSem -> NSem
  killTypedefs looking x base@(Fix (Ann num (Typedef given@(Fix (Ann _ (Name n))) _))) =
    if (num > x && looking == n) then (Fix (Ann num (TypedefT given))) else base
  killTypedefs _ _ it = it
  
  unfoldGroups :: FSem -> [FSem]
  unfoldGroups (µ -> Group a) = a >>= unfoldGroups
  unfoldGroups x = [x]

  flatGroupsAxiom :: FSem -> Sem FSem
  flatGroupsAxiom (µ -> Group a) = Group $ a >>= unfoldGroups
  flatGroupsAxiom (µ -> Program a) = Program $ a >>= unfoldGroups
  flatGroupsAxiom x = out x

  repl = repl' preprocessedC
  repl' p x = prettyPrint $ conv' p x
  
  conv = conv' preprocessedC
  conv' p x = ana flatGroupsAxiom $ convert $ parse' p x
  
  parse = parse' preprocessedC
  parse' p s = parseUnsafe (p <* eof) s
  
  source = "typedef volatile int foo; foo bar = 5; extern foo whatever; typedef volatile float bar; extern bar flisdjflkds;"
  parsed = parseUnsafe preprocessedC source
  unsanitized = convert parsed
  syntax = ana flatGroupsAxiom unsanitized
  numbered = enumerateNodes_ syntax
  result = prettyPrint syntax
  
  changeSize :: FSem -> Maybe FSem
  changeSize (Fix (Size 32)) = Just (Fix (Size 16))
  changeSize _ = Nothing
  
  has32Bit :: FSem -> Bool
  has32Bit x = case (unFix x) of
    (Size 32) -> True
    _ -> False
  
  firstTypedef nam p = head $ [ t | t@(Fix (Ann _ (Typedef (Fix (Ann _ (Name given))) _))) <- universe p, given == nam ]
  findTypedefs p = [ t | t@(Fix (Ann _ (Typedef _ _))) <- universe p ]
  typedefNames p = L.nub [ n | (Fix (Ann _ (Typedef (Fix (Ann _ (Name n))) _))) <- universe p]
  
  hello = preprocessAndParse preprocessedC "examples/hello/hello.pony.c" def
  
  testTort = do
    (Right decl) <- preprocessAndParse preprocessedC "examples/torture.pony.c" def
    let syntax = reifyTypedefs $ ana flatGroupsAxiom $ convert decl
    print $ prettyPrint syntax
  
  
  testHello = do
    (Right decl) <- preprocessAndParse preprocessedC "examples/hello/hello.pony.c" def
    let syntax = reifyTypedefs $ ana flatGroupsAxiom $ convert decl
    print $ prettyPrint syntax
  