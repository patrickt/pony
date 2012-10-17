{-# LANGUAGE NamedFieldPuns #-}

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
  import Data.Generics.Fixplate.Draw
  import Semantics.C.ASG
  import Semantics.C.Reifiable
  import Semantics.C.Reifiable.Instances
  import Semantics.C.Pretty
  import Text.PrettyPrint.GenericPretty
  
  repl = repl' preprocessedC
  repl' p x = prettyPrint $ conv' p x
  
  conv = conv' preprocessedC
  conv' p x = transform sanitize $ convert $ parseUnsafe (p <* eof) x
  
  source = "typedef volatile int foo; foo bar = 5;"
  parsed = parseUnsafe preprocessedC source
  syntax = transform sanitize $ convert parsed
  result = prettyPrint syntax
  
  topleveltypedefs = [ t | prog@(Fix (Program _)) <- universe syntax, (Fix t@(Typedef _ _)) <- children prog ]
  typedefs = [ s | Fix (Variable s@(Fix (Typedef _ _)) _ _) <- universe syntax ]
  
  sanitize :: Mu Sem -> Mu Sem
  sanitize (µ -> (Variable (µ -> (Typedef tname t)) vname val)) = Fix (Variable (Fix (TypedefT tname)) vname val)
  sanitize x = x
  