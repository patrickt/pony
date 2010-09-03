{-# LANGUAGE UnicodeSyntax, DeriveDataTypeable #-}

module Main where
  
  import Data.Data
  import Language.C
  import Language.C.System.GCC
  import System.Console.CmdArgs
  import Text.Printf
  
  data Options = Options {
    path :: FilePath
  } deriving (Show, Data, Typeable)
  
  usage :: Options
  usage = Options {
    path = def &= typFile &= argPos 0
  }
  
  arguments :: IO Options
  arguments = cmdArgs $ usage &= program "pony" &= summary "pony v0.0.0.1, (c) Patrick Thomson 2010"
  
  gccPath :: FilePath
  gccPath = "/usr/bin/llvm-gcc"
  
  compileFile :: Options -> IO ()
  compileFile (Options { path = p }) = do
    let gcc = newGCC gccPath
    parsed ← parseCFile gcc Nothing [] p
    case parsed of 
      Left err → print err
      Right trans → print $ pretty trans
  
  main :: IO ()
  main = arguments >>= compileFile
