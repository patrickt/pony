module Testing.HUnit.DeclarationPrinting 
  ( declarationTestGroup ) where
  
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Test.Framework.TH
  import Test.HUnit hiding (Test)
  import Language.Pony
  import Text.PrettyPrint.Free
  
  instance Eq (Doc a) where
    a == b = (show a) == (show b)
  
  declarationTestGroup :: Test
  declarationTestGroup = $(testGroupGenerator)
  
  case_int            = roundTrip "int a;"
  case_intarr         = roundTrip "int a[];"
  case_intarr3        = roundTrip "int a[3];"
  case_intarrx        = roundTrip "int a[x];"
  case_intarr3list    = roundTrip "int a[3] = {1, 2, 3};"
  case_intarrlist     = roundTrip "int a[] = {1, 2, 3};"
  case_intarrarrlist  = roundTrip "int a[][] = {{1, 2, 3}};"
  case_intarrarrlist2 = roundTrip "int a[2][2] = {{1, 2}, {3, 4}};"

  roundTrip :: ByteString -> Assertion
  roundTrip s = theory @=? practice where
    theory = pretty s
    practice = (para' evalPretty $ convert $ parseUnsafe preprocessedC s)
  
