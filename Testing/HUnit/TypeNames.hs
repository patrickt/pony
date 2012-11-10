module Testing.HUnit.TypeNames 
  ( tests ) where
  
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Test.HUnit hiding (Test)
  import Language.Pony
  import Text.Pretty
  import Data.Generics.Fixplate
  import qualified Data.ByteString.Char8 as B
  
  roundTrip :: ByteString -> Test
  roundTrip s = testCase (B.unpack s) $ assertEqual (B.unpack s) theory practice where
    theory = pretty s
    practice = para' evalPretty $ convert $ parseUnsafe typeName s
  
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
          , roundTrip "char[5]"]