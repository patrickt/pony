{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Testing.HUnit.Syntax 
  ( syntaxTests
  ) where
  
  import qualified Data.ByteString.Char8 as B
  import Language.Pony
  import Language.C99.QuasiQuote
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Test.Framework.TH
  import Test.HUnit hiding (Test)
  import Testing.HUnit.Asserts
  import Language.Haskell.TH
  import Language.Haskell.TH.Quote
  import Language.Haskell.TH.Syntax
  
  -- TODO more detailed tests, test for enum membership (should have three)
  case_trailingCommaEnum :: Assertion
  case_trailingCommaEnum = assertParsingSucceeds $ B.pack [here|
    enum 
    {
        red,
        blue,
        green,
    } color;
  |]
  -- 
  -- case_simpleAsm :: Assertion
  -- case_simpleAsm = theory @=? practice
  --   where theory = parseUnsafe statement "asm(\"movl %ecx %eax\")"
  --         practice = AsmStmt Nothing (Simple $ str "movl %ecx %eax")
  --         str = CStringLiteral . Constant . CString
  -- 
  case_NoSpacesNoInfixOps :: Assertion
  case_NoSpacesNoInfixOps = theory @=? practice where
    practice = parseUnsafe expression "a!=b"
    theory = binary' "a" "!=" "b"
     
  
  pointerOp = binary' (unary' "*" "a") "!=" (unary' "*" "b")
  
  case_SpacesAndInfixOps :: Assertion
  case_SpacesAndInfixOps = pointerOp @=? practice where
    practice = parseUnsafe expression "*a != *b"
  
  case_NoSpacesAndInfixOps :: Assertion
  case_NoSpacesAndInfixOps = pointerOp @=? practice where
    practice = parseUnsafe expression "*a!=*b"
  
  case_NoSpacesAndOneInfixOp :: Assertion
  case_NoSpacesAndOneInfixOp = theory @=? practice where
    practice = parseUnsafe expression "*a!=b"
    theory = binary' (unary' "*" "a") "!=" "b"
  
  syntaxTests :: [Test]
  syntaxTests = [$(testGroupGenerator)]