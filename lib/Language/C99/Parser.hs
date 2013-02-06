module Language.C99.Parser 
  ( module Text.Parsec
  , module Language.Pony.Overture
  , Parser
  , Internals(..)
  , addTypeDef
  , parseTest
  , parseTestCustom
  , parseUnsafe
  , parseFromFile
  , parseFromFileCustom
  , preprocessAndParse
  )
  where
  
  import Data.Generics
  import Language.C99.AST
  import Language.C99.Operators
  import Language.Pony.Overture
  import qualified Data.ByteString as B
  import qualified Data.Map as M
  import System.Cmd
  import Text.Parsec hiding (parseTest, many, optional, (<|>))
  import Text.Parsec.ByteString hiding (Parser, parseFromFile)
  import Text.Printf

  -- | The internal data type carried along by the parser monad: a lookup table 
  -- that maps names to types, and four arrays of strings that provide the ability
  -- to define new operators at different precedence levels.
  data Internals = Internals 
    { typedefs :: Map String CTypeName
    , operators :: [Operator]
    } deriving (Show, Eq, Typeable, Data)
  
  instance Default Internals where def = Internals def defaultOperators
  
  -- | Updates an 'Internals' record by adding a new (name, type) pair to the 
  -- lookup table. There are a number of problems with this: typedefs do not 
  -- know about their scopes (issue #41), multiple typedefs could be defined at 
  -- once, lookup tables are slow, et cetera. 
  -- tl;dr: this needs some work.
  addTypeDef :: String -> CTypeName -> Internals -> Internals
  addTypeDef name decl record = record { typedefs = M.insert name decl (typedefs record) }
  
  -- | The instance of the GenParser monad transformer over characters and carrying 'Internals'.
  type Parser = GenParser Char Internals
  
  -- | Given a parser action, a filepath, and some internals, this method runs the file through 
  -- GCC's preprocessor, parses the file, and returns an IO-wrapped type that is either 
  -- an error or the parsed type in question.
  preprocessAndParse :: Parser a -> FilePath -> Internals -> IO (Either ParseError a)
  preprocessAndParse p loc i = system preprocess >> parseFromFileCustom p "./ponytmp" i
    where preprocess = printf "/usr/bin/gcc -U __BLOCKS__ -E %s > ./ponytmp" loc
  
  -- | Given a parser action and a string, parses the string and dumps the 
  -- result to stdout. Useful for debugging.
  parseTest :: (Show a) => Parser a -> ByteString -> IO ()
  parseTest p s = parseTestCustom p s def
  
  -- | Like 'parseTest', except, you can specify a custom 'Internals' structure.
  parseTestCustom :: (Show a) => Parser a -> ByteString -> Internals -> IO ()
  parseTestCustom p s i = either print print (runParser p i "" s)
  
  -- | An unsafe variant of parseTest that actually returns the parsed object.
  -- If an error occurs, it will be printed to stdout and the function will 
  -- terminate early. Extremely useful for debugging purposes.
  parseUnsafe :: Parser a -> ByteString -> a
  parseUnsafe p s = either (error . show) id (runParser p def "" s)
  
  -- | Like 'preprocessAndParse', except without the whole preprocessing thing.
  -- Useful for debugging in conjunction with 'unsafePerformIO'.
  parseFromFile :: Parser a -> FilePath -> IO (Either ParseError a)
  parseFromFile p loc = parseFromFileCustom p loc def
  
  -- | Like 'parseFromFile', except you can specify your own 'Internals' structure.
  parseFromFileCustom :: Parser a -> FilePath -> Internals -> IO (Either ParseError a)
  parseFromFileCustom p loc internals = runParser p internals loc <$> B.readFile loc
