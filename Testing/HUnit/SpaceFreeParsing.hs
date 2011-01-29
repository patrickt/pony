module Testing.HUnit.SpaceFreeParsing 
  ( tests ) where
  
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Language.C.Parser
  import Test.HUnit hiding (Test)
  import Language.C.AST
  import Language.C.Expressions (expression)
  
  tests :: [Test]
  tests = [ testCase "no spaces but no infix operations" testNoSpacesNoInfixOps
          , testCase "spaces and infix operators" testSpacesAndInfixOps 
          , testCase "spaces and infix operators" testNoSpacesAndInfixOps
          ]
    
  testNoSpacesNoInfixOps :: Assertion
  testNoSpacesNoInfixOps = assertEqual "" theory practice where
    practice = parseUnsafe expression "a!=b"
    theory = BinaryOp "!=" (Identifier "a") (Identifier "b")
    
  testSpacesAndInfixOps :: Assertion
  testSpacesAndInfixOps = assertEqual "" theory practice where
    practice = parseUnsafe expression "*a != *b"
    theory = BinaryOp "!=" (UnaryOp "*" (Identifier "a")) (UnaryOp "*" (Identifier "b"))
  
  testNoSpacesAndInfixOps :: Assertion
  testNoSpacesAndInfixOps = assertEqual "" theory practice where
    practice = parseUnsafe expression "*a!=*b"
    theory = BinaryOp "!=" (UnaryOp "*" (Identifier "a")) (UnaryOp "*" (Identifier "b"))