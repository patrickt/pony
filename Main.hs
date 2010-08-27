{-# LANGUAGE UnicodeSyntax, DeriveDataTypeable #-}

module Main where
  
  import Data.Data
  import System.Console.CmdArgs
  import Text.Printf
  
  data Options = Options {
    path :: FilePath
  } deriving (Show, Data, Typeable)
  
  usage = Options {
    path = def &= typFile &= argPos 0
  }
  
  arguments = cmdArgs $ usage &= program "pony" &= summary "pony v0.0.0.1, (c) Patrick Thomson 2010"
  
  compileFile :: Options -> IO Bool
  compileFile opts = do
    printf "going to compile file '%s'." (path opts)
    return True
  
  main :: IO ()
  main = (print =<< compileFile =<< arguments) >> return ()