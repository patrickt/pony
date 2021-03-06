{-# LANGUAGE NamedFieldPuns, OverlappingInstances, ViewPatterns #-}

module Language.Pony 
  ( module Semantics.C
  , module Data.Functor.Fix
  , module Language.Pony.Overture
  , run
  , PonyOptions (..)
  ) 
  
  where
    -- 
  
  import Data.Functor.Fix hiding (foldl, sequence, mapM)
  import Language.C99 
  import Language.Pony.Overture
  import Language.Pony.Transformations.Sanitizers
  import Semantics.C
  import System.Environment
  import System.Exit

  
  data PonyOptions = PonyOptions
    { topDown :: [Fix Sem -> Fix Sem]
    , anamorphisms :: [Fix Sem -> Sem (Fix Sem)]
    , bitwiseOperators :: [String]
    }
  
  instance Default PonyOptions where def = PonyOptions [] [] []
  
  run :: PonyOptions -> IO ()
  run (PonyOptions top anas bwo) = do
    args <- getArgs
    when (length args == 0) $ do
      putStrLn "Error: filename not provided"
      exitFailure
    parsed <- preprocessAndParse preprocessedC (args !! 0) (def { bitwiseOps = bwo })
    case parsed of 
      (Left a) -> putStrLn "ERROR" >> print a >> exitFailure
      (Right ast) -> do
        let asg = runReplacer $ convert ast
        let anamorphed = foldl (flip ana) asg anas
        let topdowned = foldl apply anamorphed top where apply = flip ($)
        let prettied = prettyPrint topdowned
        print prettied
  

  