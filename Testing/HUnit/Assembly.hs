module Testing.HUnit.Assembly 
  ( tests ) where
  
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Test.HUnit hiding (Test)
  import Language.C99
  import Language.C99.Literals
  
  tests :: [Test]
  tests = [ testCase "simple statements" testSimpleStatements
          ]
          
  str = CStringLiteral . Constant . CString
  
  testSimpleStatements :: Assertion
  testSimpleStatements = assertEqual "asm(stmt)" theory practice
    where theory = parseUnsafe statement "asm(\"movl %ecx %eax\")"
          practice = AsmStmt Nothing (Simple $ str "movl %ecx %eax")
          
  
