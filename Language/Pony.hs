{-# LANGUAGE NamedFieldPuns, OverlappingInstances, ViewPatterns, OverloadedStrings #-}

module Language.Pony 
  ( module Semantics.C.ASG
  , module Semantics.C.Pretty
  , module Semantics.C.Reifiable
  , module Language.C99
  , module Data.Functor.Fix
  ) 
  
  where
    -- 
  import Control.Monad.State
  import Data.Functor.Fix
  import Language.C99 hiding (CChar, CFloat, Empty, State, parse, attribute, typedefs)
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
  import Semantics.C.ASG.Newtypes
  import Semantics.C.ASG.Arbitrary
  import Test.QuickCheck
  import Debug.Trace
  
  type Recorder = State [FSem]
  
  bool :: a -> a -> Bool -> a
  bool a b p = if p then a else b
  
  replaceTypedefs :: FSem -> Recorder FSem
  replaceTypedefs x@(Fix (Typedef name@(Fix (Name _)) _)) = do
    contained <- gets (elem x)
    unless contained (modify (x :))
    return $ bool (tie $ TypedefT name) x contained
  replaceTypedefs x = pure x

  runReplacer :: FSem -> FSem
  runReplacer x = evalState (replaceTypedefs `transformM` x) []
  
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
  parse' p = parseUnsafe (p <* eof)
  
  source = "typedef volatile int foo; foo bar = 5; extern foo whatever; typedef volatile float bar; extern bar flisdjflkds;"
  parsed = parseUnsafe preprocessedC source
  unsanitized = convert parsed
  syntax = runReplacer unsanitized
  numbered = enumerateNodes_ syntax
  result = prettyPrint syntax
  
  hello = preprocessAndParse preprocessedC "examples/hello/hello.pony.c" def
  
  testTort = do
    (Right decl) <- preprocessAndParse preprocessedC "examples/torture.pony.c" def
    let syntax = runReplacer $ ana flatGroupsAxiom $ convert decl
    print $ prettyPrint syntax
  
  
  testHello = do
    (Right decl) <- preprocessAndParse preprocessedC "examples/hello/hello.pony.c" def
    let syntax = runReplacer $ ana flatGroupsAxiom $ convert decl
    print $ prettyPrint syntax
  