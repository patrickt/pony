{-# LANGUAGE UnicodeSyntax, DeriveDataTypeable, StandaloneDeriving, NamedFieldPuns #-}

module Main where
  
  import Data.Data
  import Language.C
  import Language.C.System.GCC
  import Language.Pony.Parser
  import System.Console.CmdArgs
  import Text.Parsec
  
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
  parsePony (Options { path }) = readFile path >>= parseTest languageDeclarator
    
  main :: IO ()
  main = arguments >>= parsePony
