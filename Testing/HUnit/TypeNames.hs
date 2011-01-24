module Testing.HUnit.TypeNames 
  ( tests ) where
  
  import Language.C.Declarations
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Language.C.Parser
  import Test.HUnit hiding (Test)
  import Data.Either
  import Data.Maybe
  import Semantics.C.Conversions
  import Semantics.C.Nodes
  
  tests :: [Test]
  tests = [ testCase "whatever" testRestrictPtrToConstPtrToInt ]
    
  testRestrictPtrToConstPtrToInt :: Assertion
  testRestrictPtrToConstPtrToInt = assertEqual "whatever" theory practice where
    practice = fromJust $ convertDeclarationToType $ parseUnsafe typeName "const int * const * restrict "
    theory = SPointerTo (SPointerTo (SInt (IntegerFlags Signed 32) [Const]) [Const]) [Restrict]
  
  