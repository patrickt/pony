{-# LANGUAGE NamedFieldPuns, OverlappingInstances #-}

module Language.Pony 
  ( module Semantics.C.ASG
  , module Semantics.C.Pretty
  , module Semantics.C.Reifiable
  , module Language.C99
  ) 
  
  where
    -- 
  import Data.Functor.Fix
  import Language.C99 hiding (CChar, CFloat, Empty)
  import Data.Generics.Fixplate.Attributes
  import Data.Generics.Fixplate.Draw
  import Data.Generics.Fixplate.Traversals
  import Semantics.C.ASG
  import Semantics.C.Reifiable
  import Semantics.C.Reifiable.Instances
  import Semantics.C.Pretty
  import Text.PrettyPrint.GenericPretty
  import qualified Data.Foldable as F
  import Semantics.C.Queries

  
  repl = repl' preprocessedC
  repl' p x = prettyPrint $ conv' p x
  
  conv = conv' preprocessedC
  conv' p x = convert $ parseUnsafe (p <* eof) x
  
  source = "typedef volatile int foo; foo bar = 5;"
  parsed = parseUnsafe preprocessedC source
  syntax = rewrite sanitize $ convert parsed
  result = prettyPrint syntax
  
  changeSize :: FSem -> Maybe FSem
  changeSize (Fix (Size 32)) = Just (Fix (Size 16))
  changeSize _ = Nothing
  
  has32Bit :: FSem -> Bool
  has32Bit x = case (unFix x) of
    (Size 32) -> True
    _ -> False
  
  topleveltypedefs = [ t | prog@(Fix (Program _)) <- universe syntax, (Fix t@(Typedef _ _)) <- children prog ]
  typedefs = [ s | Fix (Variable s@(Fix (Typedef _ _)) _ _) <- universe syntax ]
  
  sanitize :: Mu Sem -> Maybe (Mu Sem)
  sanitize (µ -> (Typedef tname (µ -> (Typedef tname' _)))) = Just $ Fix (Typedef tname tname')
  sanitize (µ -> (Variable (µ -> (Typedef tname t)) vname val)) = Just $ Fix (Variable (Fix (TypedefT tname)) vname val)
  sanitize _ = Nothing
  
  hello = preprocessAndParse preprocessedC "examples/hello/hello.pony.c" def
  