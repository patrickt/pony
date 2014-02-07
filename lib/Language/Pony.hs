{-# LANGUAGE OverlappingInstances #-}

module Language.Pony 
  ( module Control.Lens
  , module Language.Pony.Overture
  , module Language.C99
  , run
  , PonyOptions (..)
  ) 
  
  where
    -- 
  
  import qualified Data.ByteString.Char8 as B
  import Language.C99 hiding (uncons)
  import Language.Pony.Overture
  import System.Environment
  import System.Exit
  import Text.PrettyPrint.Free
  import Language.C99.QuasiQuote
  import System.Random
  import Control.Lens hiding (noneOf, Const)
  import Language.C11.Syntax
  
  data PonyOptions = PonyOptions
  
  run :: a -> IO ()
  run x = exitSuccess
  
  -- data PonyOptions = PonyOptions
  --   { topDown :: [CSyn -> CSyn]
  --   , anamorphisms :: [CSyn -> C99 CSyn]
  --   , binaryOperators :: [Operator]
  --   }
  -- 
  -- instance Default PonyOptions where def = PonyOptions [] [] defaultOperators
  -- 
  -- run :: PonyOptions -> IO ()
  -- run (PonyOptions top anas bwo) = do
  --   args <- getArgs
  --   rand <- getStdGen
  --   when (length args == 0) $ do
  --     putStrLn "Error: filename not provided"
  --     exitFailure
  --   parsed <- preprocessAndParse preprocessedC (args !! 0) (def { operators = bwo, seed = Just rand })
  --   case parsed of 
  --     (Left a) -> putStrLn "ERROR" >> print a >> exitFailure
  --     (Right asg) -> do
  --       let anamorphed = foldl (flip ana) asg anas
  --       let topdowned = foldl apply anamorphed top where apply = flip ($)
  --       let prettied = prettyPrint topdowned
  --       putDoc prettied
  

  