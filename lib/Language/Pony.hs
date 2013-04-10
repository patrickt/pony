{-# LANGUAGE OverlappingInstances #-}

module Language.Pony 
  ( module Data.Functor.Fix
  , module Language.Pony.Overture
  , module Language.C99
  , run
  , PonyOptions (..)
  ) 
  
  where
    -- 
  
  import Data.Functor.Fix
  import Language.C99
  import Language.Pony.Overture
  import Language.Pony.Transformations.Sanitizers
  import System.Environment
  import System.Exit
  import Text.PrettyPrint.Free

  
  data PonyOptions = PonyOptions
    { topDown :: [CSyn -> CSyn]
    , anamorphisms :: [CSyn -> C99 CSyn]
    , binaryOperators :: [Operator]
    }
  
  instance Default PonyOptions where def = PonyOptions [] [] []
  
  run :: PonyOptions -> IO ()
  run (PonyOptions top anas bwo) = do
    args <- getArgs
    when (length args == 0) $ do
      putStrLn "Error: filename not provided"
      exitFailure
    parsed <- preprocessAndParse preprocessedC (args !! 0) (def { operators = bwo })
    case parsed of 
      (Left a) -> putStrLn "ERROR" >> print a >> exitFailure
      (Right ast) -> do
        putDoc $ prettyPrint ast
  

  