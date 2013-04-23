module Testing.HUnit.Typedefs
  ( tests
  ) where
  
  import Data.Generics.Fixplate
  import Language.Pony
  import qualified Data.ByteString.Char8 as B
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Test.Framework.TH
  import Test.HUnit hiding (Test)
  import Text.PrettyPrint.Free
  
  roundTrip :: ByteString -> Assertion
  roundTrip s = theory @=? practice where
    theory = pretty s
    practice = (para' evalPretty $ parseUnsafe preprocessedC s)

  roundTrip' :: [ByteString] -> Assertion
  roundTrip' bs = roundTrip $ B.intercalate "\n" bs 
  
  
  case_scalar       = roundTrip  "typedef int foo;"
  case_pointer      = roundTrip  "typedef float **foo;"
  case_anonstruct   = roundTrip' [ "typedef struct {"
                                 , "  int field;"
                                 , "} anon_struct;" ]
  case_namedstruct  = roundTrip' [ "typedef struct foo {"
                                 , "  int field;"
                                 , "} named_foo_struct;" ]
  case_anonenum     = roundTrip "typedef enum {EnumValue} anon_enum;"
  case_namedenum    = roundTrip "typedef enum foo {EnumValue} named_foo_enum;"
  case_outboardenum = roundTrip' [ "enum foo {EnumValue};"
                                 , "typedef enum foo outboard_enum;" ]
                                  
  
  tests :: [Test]
  tests = [$(testGroupGenerator)]
