module Testing.HUnit.DeclarationPrinting 
  ( tests ) where
  
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Test.HUnit hiding (Test)
  import Language.Pony
  import Text.Pretty
  import Data.Generics.Fixplate
  import qualified Data.ByteString.Char8 as B
  
  tests :: [Test]
  tests = [ roundTrip "int a;"
          , roundTrip "int a[];"
          , roundTrip "int a[3];"
          , roundTrip "int a[x];"
          , roundTrip "int a[3] = {1, 2, 3};"
          , roundTrip "int a[] = {1, 2, 3};"
          , roundTrip "int a[][] = {{1, 2, 3}};"
          , roundTrip "int a[2][2] = {{1, 2}, {3, 4}};"
          ]

  roundTrip :: ByteString -> Test
  roundTrip s = testCase (B.unpack s) $ assertEqual (B.unpack s) theory practice where
    theory = pretty s
    practice = (para' evalPretty $ convert $ parseUnsafe preprocessedC s)
  
