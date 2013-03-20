module Testing.HUnit.Syntax 
  ( syntaxTestGroup
  ) where
  
  import qualified Data.ByteString.Char8 as B
  import Language.Pony
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Test.Framework.TH
  import Test.HUnit hiding (Test)
  import Testing.HUnit.Asserts
  
  -- TODO more detailed tests, test for enum membership (should have three)
  -- case_trailingCommaEnum :: Assertion
  -- case_trailingCommaEnum = assertParsingSucceeds $ B.pack [here|
  --   enum 
  --   {
  --       red,
  --       blue,
  --       green,
  --   } color;
  -- |]
  -- 
  -- case_simpleAsm :: Assertion
  -- case_simpleAsm = theory @=? practice
  --   where theory = parseUnsafe statement "asm(\"movl %ecx %eax\")"
  --         practice = AsmStmt Nothing (Simple $ str "movl %ecx %eax")
  --         str = CStringLiteral . Constant . CString
  -- 
  -- case_NoSpacesNoInfixOps :: Assertion
  -- case_NoSpacesNoInfixOps = theory @=? practice where
  --   practice = parseUnsafe expression "a!=b"
  --   theory = BinaryOp "!=" (Identifier "a") (Identifier "b")
  --   
  -- case_SpacesAndInfixOps :: Assertion
  -- case_SpacesAndInfixOps = theory @=? practice where
  --   practice = parseUnsafe expression "*a != *b"
  --   theory = BinaryOp "!=" (UnaryOp "*" (Identifier "a")) (UnaryOp "*" (Identifier "b"))
  -- 
  -- case_NoSpacesAndInfixOps :: Assertion
  -- case_NoSpacesAndInfixOps = theory @=? practice where
  --   practice = parseUnsafe expression "*a!=*b"
  --   theory = BinaryOp "!=" (UnaryOp "*" (Identifier "a")) (UnaryOp "*" (Identifier "b"))
  -- 
  -- case_NoSpacesAndOneInfixOp :: Assertion
  -- case_NoSpacesAndOneInfixOp = theory @=? practice where
  --   practice = parseUnsafe expression "*a!=b"
  --   theory = BinaryOp "!=" (UnaryOp "*" (Identifier "a")) (Identifier "b")
  
  syntaxTestGroup :: Test
  syntaxTestGroup = $(testGroupGenerator)