{-# LANGUAGE NamedFieldPuns #-}

module Language.Pony 
  ( module Semantics.C.ASG
  , run
  , pony
  ) 
  
  where
    -- 
    -- import Data.Generics hiding (empty)
  import Language.C99
    -- import Language.C99.Literals
    -- import Language.Pony.Transformations
    -- import Language.Pony.Transformations.Utilities
  import Semantics.C.ASG
  import Semantics.C.Reifiable
  import Semantics.C.Reifiable.Instances
  -- import System.Environment
  
  pony :: IO ()
  pony = print "yay"
  
  run :: PonyOptions -> IO ()
  run _ = print "woo"
  
  data PonyOptions = PonyOptions
  