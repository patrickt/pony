{-# LANGUAGE NamedFieldPuns #-}

module Language.Pony 
  ( module Data.Generics
  , module Language.Pony.Transformations
  , module Language.Pony.Transformations.Utilities
  , module Semantics.C
  , run
  , pony
  , Operator (..)
  , PonyOptions (..)
  ) 
  
  where
  
  import Data.Generics hiding (empty)
  import Language.C99
  import Language.Pony.Transformations
  import Language.Pony.Transformations.Utilities
  import Semantics.C
  import System.Environment
  
  data Operator 
    = Arithmetic String
    | Bitwise String 
    | Comparative String
    | Logical String
  
  run :: PonyOptions -> IO ()
  run PonyOptions { output, operators, transformations } = do
    -- todo support more than one output file.
    args <- getArgs
    let input = last args
    let internals = internalsFromOperators operators
    let trans = head transformations
    result <- preprocessAndParse preprocessedC input internals
    case result of
      (Left parseError) -> print parseError
      Right externs -> do
        let converted = convert externs
        let (MkTrans _ direction t) = trans
        case direction of 
          TopDown -> writeFile output (show $ pretty $ everywhere t converted)
          BottomUp -> writeFile output (show $ pretty $ everywhere' t converted)
  
  internalsFromOperators :: [Operator] -> Internals
  internalsFromOperators ops = emptyInternals {
    arithmeticOps = ariths,
    comparativeOps = compars,
    bitwiseOps = bitwises,
    logicalOps = logics
  } where 
    ariths = [ a | (Arithmetic a) <- ops ]
    compars = [ c | (Comparative c) <- ops ]
    bitwises = [ b | (Bitwise b) <- ops ]
    logics = [ q | (Logical q) <- ops ]
  
  data PonyOptions = PonyOptions 
    { output :: FilePath
    , operators :: [Operator]
    , transformations :: [Transformation]
    }
  
  pony :: PonyOptions
  pony = PonyOptions 
    { output          = "/dev/stdout"
    , operators       = []
    , transformations = []
    }