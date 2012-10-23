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
  
  source = "typedef volatile int foo; foo bar = 5; extern foo whatever;"
  parsed = parseUnsafe preprocessedC source
  unsanitized = convert parsed
  syntax = ana flatGroupsAxiom unsanitized
  result = prettyPrint syntax
  
  changeSize :: FSem -> Maybe FSem
  changeSize (Fix (Size 32)) = Just (Fix (Size 16))
  changeSize _ = Nothing
  
  has32Bit :: FSem -> Bool
  has32Bit x = case (unFix x) of
    (Size 32) -> True
    _ -> False
  
  topleveltypedefs p = [ t | prog@(Fix (Program _)) <- universe p, (Fix t@(Typedef _ _)) <- children prog]
  
  hello = preprocessAndParse preprocessedC "examples/hello/hello.pony.c" def
  
  
  testHello = do
    (Right decl) <- preprocessAndParse preprocessedC "examples/hello/hello.pony.c" def
    let syntax = ana flatGroupsAxiom $ convert decl
    print $ prettyPrint syntax
  