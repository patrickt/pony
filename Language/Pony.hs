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
  
  import Language.Pony.Overture
  import Data.Functor.Fix hiding (foldl, sequence, mapM)
  import Language.C99 (preprocessAndParse, preprocessedC)
  import Semantics.C
  import System.Environment
  import System.Exit
  
  data PonyOptions = PonyOptions
    { topDown :: [Fix Sem -> Fix Sem]
    , anamorphisms :: [Fix Sem -> Sem (Fix Sem)]
    }
  
  instance Default PonyOptions where def = PonyOptions [] []
  
  run :: PonyOptions -> IO ()
  run (PonyOptions top anas) = do
    args <- getArgs
    when (length args == 0) $ do
      putStrLn "Error: filename not provided"
      exitFailure
    parsed <- preprocessAndParse preprocessedC (args !! 0) def
    case parsed of 
      (Left a) -> putStrLn "ERROR" >> print a >> exitFailure
      (Right ast) -> do
        let asg = convert ast
        let anamorphed = foldl (flip ana) asg anas
        let topdowned = foldl apply anamorphed top where apply = flip ($)
        let prettied = prettyPrint topdowned
        print prettied
  

  