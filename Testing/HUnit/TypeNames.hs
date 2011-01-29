module Testing.HUnit.TypeNames 
  ( tests ) where
  
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Test.HUnit hiding (Test)
  import Language.C
  import Semantics.C.Conversions
  import Semantics.C.Nodes
  import Data.Maybe (fromJust)
  
  tests :: [Test]
  tests = [ testCase "whatever" testRestrictPtrToConstPtrToInt ]
    
  testRestrictPtrToConstPtrToInt :: Assertion
  testRestrictPtrToConstPtrToInt = assertEqual "whatever" theory practice where
    practice = fromJust $ convertDeclarationToType $ parseUnsafe typeName "const int * const * restrict "
    theory = SPointerTo (SPointerTo (SInt (IntegerFlags Signed 32) [Const]) [Const]) [Restrict]
  
  