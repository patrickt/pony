module Testing.HUnit.TypeNames 
  ( tests ) where
  
  import Data.Either
  import Data.Generics.Fixplate
  import Language.C99
  import Language.Haskell.TH
  import Language.Haskell.TH.Quote
  import Language.Pony
  import qualified Data.ByteString.Char8 as B
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Test.HUnit hiding (Test)
  import Testing.HUnit.Asserts
  import Testing.Heredoc
  import Text.Pretty

  
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
  
  innerScopeGood :: B.ByteString
  innerScopeGood = B.pack [here|
    void foo() {
      typedef char TT;
      TT x;
    }
    void bar() {
        unsigned TT;
    } |]
  
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
          , assertParsingSucceeds "good inner scope" innerScopeGood
          , assertParsingFails "bad inner scope" innerScopeBad
          ]