{-# LANGUAGE TemplateHaskell #-}

module Language.Pony.Testing where
  
  import Language.C99 hiding (CChar, CFloat, Empty, State, parse, attribute, typedefs)
  import Language.Pony.Transformations.Sanitizers
  import Language.Pony.Overture
  import Data.Functor.Fix
  import Data.Generics.Fixplate.Attributes
  import Data.Generics.Fixplate.Draw
  import Data.Generics.Fixplate.Traversals
  import Semantics.C.ASG
  import Semantics.C.Reifiable
  import Semantics.C.Reifiable.Instances
  import Semantics.C.Pretty
  import Text.PrettyPrint.GenericPretty
  import Semantics.C.ASG.Newtypes
  import Semantics.C.ASG.Arbitrary
  import Debug.Trace
  import Semantics.C.QuasiQuote

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
  