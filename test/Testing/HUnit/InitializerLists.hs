module Testing.HUnit.InitializerLists 
  ( tests ) where
  
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Test.Framework.TH
  import Test.HUnit hiding (Test)
  import Language.Pony
  import Text.PrettyPrint.Free
  
  tests :: [Test]
  tests = [$(testGroupGenerator)]
  
  case_intarr3list    = roundTrip "int a[3] = {1, 2, 3};"
  case_intarrlist     = roundTrip "int a[] = {1, 2, 3};"
  case_intarrarrlist  = roundTrip "int a[][] = {{1, 2, 3}};"
  case_intarrarrlist2 = roundTrip "int a[2][2] = {{1, 2}, {3, 4}};"

  roundTrip :: ByteString -> Assertion
  roundTrip s = theory @=? practice where
    theory = pretty s
    practice = (para' evalPretty $ parseUnsafe preprocessedC s)
  
