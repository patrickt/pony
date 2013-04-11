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
  
  import qualified Data.ByteString.Char8 as B
  import Data.Functor.Fix
  import Language.C99
  import Language.Pony.Overture
  import Language.Pony.Transformations.Sanitizers
  import System.Environment
  import System.Exit
  import Text.PrettyPrint.Free
  import Language.C99.QuasiQuote
  import System.Random
  
  t = parseUnsafe preprocessedC $ B.pack [here|
    char *funopen(const void *,
                     int (*)(void *, char *, int),
                     int (*)(void *, const char *, int),
                     float (*)(void *, float, int),
                     int (*)(void *));
  |]
  
  data PonyOptions = PonyOptions
    { topDown :: [CSyn -> CSyn]
    , anamorphisms :: [CSyn -> C99 CSyn]
    , binaryOperators :: [Operator]
    }
  
  instance Default PonyOptions where def = PonyOptions [] [] defaultOperators
  
  run :: PonyOptions -> IO ()
  run (PonyOptions top anas bwo) = do
    args <- getArgs
    rand <- getStdGen
    when (length args == 0) $ do
      putStrLn "Error: filename not provided"
      exitFailure
    parsed <- preprocessAndParse preprocessedC (args !! 0) (def { operators = bwo, seed = Just rand })
    case parsed of 
      (Left a) -> putStrLn "ERROR" >> print a >> exitFailure
      (Right asg) -> do
        let anamorphed = foldl (flip ana) asg anas
        let topdowned = foldl apply anamorphed top where apply = flip ($)
        let prettied = prettyPrint topdowned
        putDoc prettied
  

  