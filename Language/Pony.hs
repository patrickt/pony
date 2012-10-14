{-# LANGUAGE NamedFieldPuns #-}

module Language.Pony 
  ( module Semantics.C.ASG
  , module Data.Functor.Fix
  , module Semantics.C.Pretty
  , module Semantics.C.Reifiable
  , module Language.C99
  ) 
  
  where
    -- 
  import Data.Functor.Fix
    -- import Data.Generics hiding (empty)
  import Language.C99 hiding (CChar, CFloat, Empty)
    -- import Language.C99.Literals
    -- import Language.Pony.Transformations
    -- import Language.Pony.Transformations.Utilities
  import Semantics.C.ASG
  import Semantics.C.Reifiable
  import Semantics.C.Reifiable.Instances
  import Semantics.C.Pretty
  -- import System.Environment
  import Text.PrettyPrint.GenericPretty
  
  repl = repl' preprocessedC
  repl' p x = prettyPrint $ conv' p x
  
  conv = conv' preprocessedC
  conv' p x = convert $ parseUnsafe (p <* eof) x
  
  fuck = "static const int (*func)(int, float, char);"
  astfuck = parseUnsafe preprocessedC fuck
