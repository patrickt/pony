{-# LANGUAGE DeriveDataTypeable, NamedFieldPuns, ScopedTypeVariables #-}

module Main where
  
  import Data.Generics
  import Language.C
  import Language.Pony.Transformations
  import System.Environment
  
  getArguments :: IO (FilePath, FilePath)
  getArguments = do
    args <- getArgs
    return (args !! 0, args !! 1)
  
  parsePony :: (FilePath, FilePath) -> IO ()
  parsePony (input, output) = do
    result <- parseCFilePre input
    case result of
      Left parseError -> print "error!"
      Right translUnit -> do
        let transformed = everywhere (mkT mallocToXmalloc) translUnit
        let cCode = show $ pretty transformed
        if (output == "stdout")
          then print $ pretty transformed
          else writeFile output cCode
    
  main :: IO ()
  main = getArguments >>= parsePony
