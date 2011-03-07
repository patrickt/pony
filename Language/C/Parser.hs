module Language.C.Parser 
  ( module Text.Parsec
  , module Control.Applicative
  , Parser
  , Internals(..)
  , emptyInternals
  , addTypeDef
  , parseTest
  , parseTestCustom
  , parseUnsafe
  , parseFromFile
  , parseFromFileCustom
  , preprocessAndParse
  )
  where
  
  import Control.Monad
  import Control.Applicative hiding (Const)
  import Data.Generics
  import Language.C.AST
  import Language.C.Literals
  import System.Cmd
  import Text.Parsec hiding (parseTest, many, optional, (<|>))
  import Text.Parsec.String hiding (Parser, parseFromFile)
  import Text.Printf

  
  data Internals = Internals 
    { typedefs :: [(String, CDeclaration)]
    , arithmeticOps :: [String]
    , comparativeOps :: [String]
    , bitwiseOps :: [String]
    , logicalOps :: [String]
    }
  
  emptyInternals :: Internals
  emptyInternals = Internals { typedefs       = []
                             , arithmeticOps  = [] 
                             , comparativeOps = []
                             , bitwiseOps     = []
                             , logicalOps     = []
                             }
  
  addTypeDef :: String -> CDeclaration -> Internals -> Internals
  addTypeDef name decl record = record { typedefs = typedefs record ++ [(name, decl)]}
  
  type Parser = GenParser Char Internals
  
  preprocessAndParse :: Parser a -> FilePath -> Internals -> IO (Either ParseError a)
  preprocessAndParse p loc i = system preprocess >> parseFromFileCustom p "./ponytmp" i
    where preprocess = printf "/usr/bin/gcc -U __BLOCKS__ -E %s > ./ponytmp" loc
  
  parseTest :: (Show a) => Parser a -> String -> IO ()
  parseTest p s = parseTestCustom p s emptyInternals
  
  parseTestCustom :: (Show a) => Parser a -> String -> Internals -> IO ()
  parseTestCustom p s i = either print print (runParser p i "" s)
  
  parseUnsafe :: Parser a -> String -> a
  parseUnsafe p s = either (error . show) id (runParser p emptyInternals "" s)
  
  parseFromFile :: Parser a -> FilePath -> IO (Either ParseError a)
  parseFromFile p loc = parseFromFileCustom p loc emptyInternals
  
  parseFromFileCustom :: Parser a -> FilePath -> Internals -> IO (Either ParseError a)
  parseFromFileCustom p loc internals = runParser p internals loc <$> readFile loc
