module Testing.HUnit.TypeNames 
  ( tests ) where
  
  import Data.Either
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Test.HUnit hiding (Test)
  import Language.Haskell.TH
  import Language.Haskell.TH.Quote
  import Language.Pony
  import Language.C99
  import Text.Pretty
  import Data.Generics.Fixplate
  import Testing.Heredoc
  import qualified Data.ByteString.Char8 as B
  
  
  roundTrip :: ByteString -> Test
  roundTrip s = testCase (B.unpack s) $ assertEqual (B.unpack s) theory practice where
    theory = pretty s
    practice = para' evalPretty $ convert $ parseUnsafe typeName s
  
  -- should fail since TT is not a type in bar
  innerScopeBad :: B.ByteString
  innerScopeBad = B.pack [here|
    void foo() {
      typedef char TT;
      TT x;
    }
    void bar() {
      TT y;
    } |]
  
  assertLeft :: (Show b) => Either a b -> Assertion
  assertLeft (Left _) = assertBool "" True
  assertLeft (Right x) = assertFailure ("Got Right " <> show x <> ", expected a Left")
  
  assertRight :: (Show a) => Either a b -> Assertion
  assertRight (Right _) = assertBool "" True
  assertRight (Left x) = assertFailure ("Got Left " <> show x <> ", expected a Right")
  
  innerScopeGood :: B.ByteString
  innerScopeGood = B.pack [here|
    void foo() {
      typedef char TT;
      TT x;
    }
    void bar() {
        unsigned TT;
    } |]

  parsingSucceeds :: String -> ByteString -> Test
  parsingSucceeds reason code = testCase reason $ assertRight (runParser preprocessedC def "test" code)
  
  parsingFails :: String -> ByteString -> Test
  parsingFails reason code = testCase reason $ assertLeft (runParser preprocessedC def "test" code)
  
  tests :: [Test]
  tests = [ roundTrip "int"
          , roundTrip "int *"
          , roundTrip "const int"
          , roundTrip "const int *"
          , roundTrip "static const int"
          , roundTrip "static const int *"
          , roundTrip "int * const"
          , roundTrip "const int * const"
          , roundTrip "static const int * const"
          , roundTrip "int * *"
          , roundTrip "int * const *"
          , roundTrip "const int * const *"
          , roundTrip "const int * const * restrict"
          , roundTrip "char"
          , roundTrip "char[]"
          , roundTrip "char[5]"
          , parsingSucceeds "good inner scope" innerScopeGood
          , parsingFails "bad inner scope" innerScopeBad
          ]