{-# LANGUAGE UnicodeSyntax, DeriveDataTypeable, StandaloneDeriving, NamedFieldPuns, ScopedTypeVariables #-}

module Main where
  
  import Data.Data
  import Language.C
  import Language.C.System.GCC
  import Language.Pony.Node
  import Data.Tree
  import System.Console.CmdArgs
  
  data Options = Options {
    path :: FilePath
  } deriving (Show, Data, Typeable)
  
  usage :: Options
  usage = Options {
    path = def &= typFile &= argPos 0
  }
  
  arguments :: IO Options
  arguments = cmdArgs $ usage &= program "pony" &= summary "pony v0.0.0.1, (c) Patrick Thomson 2010"
  
  
  parsePony :: Options -> IO ()
  parsePony (Options { path }) = do
    result <- parseCFilePre path
    either print (print . cNodeToTree) result
    
  main :: IO ()
  main = do
    arguments >>= parsePony
