{-# LANGUAGE OverlappingInstances #-}

module Language.Pony 
  ( module Semantics.C
  , module Data.Functor.Fix
  , module Language.Pony.Overture
  , module Language.C99
  , run
  , PonyOptions (..)
  ) 
  
  where
    -- 
  
  import Data.Functor.Fix hiding (foldl, sequence, mapM, attribute)
  import Language.C99 hiding (CChar, CFloat, Empty)
  import Language.Pony.Overture
  import Language.Pony.Transformations.Sanitizers
  import Semantics.C
  import System.Environment
  import System.Exit

  
  data PonyOptions = PonyOptions
    { topDown :: [Fix Sem -> Fix Sem]
    , anamorphisms :: [Fix Sem -> Sem (Fix Sem)]
    , bitwiseOperators :: [String]
    , arbitraryIO :: [Fix Sem -> IO ()]
    }
  
  instance Default PonyOptions where def = PonyOptions [] [] [] []
  
  run :: PonyOptions -> IO ()
  run (PonyOptions top anas bwo arb) = do
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
        let doAllIO x (a:as) = a x >> doAllIO x as
            doAllIO _ [] = return ()
        let arbitraried = doAllIO anamorphed arb
        arbitraried
        print prettied
  

  