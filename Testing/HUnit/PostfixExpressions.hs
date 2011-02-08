module Testing.HUnit.PostfixExpressions 
  ( tests ) where
  
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Test.HUnit hiding (Test)
  import Language.C
  
  tests :: [Test]
  tests = [ testCase "multiple arrows" testMultipleArrows
          ]
  
  testMultipleArrows :: Assertion
  testMultipleArrows = assertEqual "multiple arrows" theory practice
    where theory = parseUnsafe postfixExpression "a->b->c"
          practice = BinaryOp "->" (BinaryOp "->" (ident "a") (ident "b")) (ident "c")
          ident = Identifier
  