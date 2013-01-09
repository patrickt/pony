module Testing.HUnit.UtilityFunctions 
  ( tests ) where
  
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Test.HUnit hiding (Test)
  import Language.Pony
  import Language.Pony.Overture
  import Language.C99
  import qualified Data.ByteString.Char8 as B
  
  isTypedef :: Bool -> ByteString -> Test
  isTypedef b code = testCase (B.unpack code) $ assertEqual "declarationIsTypedef is wrong" b $ declarationIsTypedef d where
    (CTranslationUnit [ExternDecl d]) = parseUnsafe preprocessedC code
  
  isComposite :: Bool -> ByteString -> Test
  isComposite b code = testCase (B.unpack code) $ assertEqual "declarationIsComposite is wrong" b $ declarationIsComposite d where
    (CTranslationUnit [ExternDecl d]) = parseUnsafe preprocessedC code
  
  hasFields b code = testCase (B.unpack code) $ assertEqual "declarationHasFields is wrong" b $ declarationHasFields d where
    (CTranslationUnit [ExternDecl d]) = parseUnsafe preprocessedC code
  
  tests :: [Test]
  tests = [ isTypedef True simpleTypedef
          , isTypedef True compositeTypedef
          , isTypedef True functionPointerTypedef
          , isTypedef False staticInt
          , isComposite True fooStruct
          , isComposite True clipseUnion
          , isComposite True forwardStruct
          , isComposite False staticInt
          , hasFields True fooStruct
          , hasFields True clipseUnion
          , hasFields True compositeTypedef
          , hasFields False staticInt
          , hasFields False forwardStruct
          ]
        where
          simpleTypedef = "typedef int foo;"
          compositeTypedef = "typedef struct foo_s { int blah; int baz; } foo_t;"
          functionPointerTypedef = "typedef int (*clever)(float monkeys);"
          forwardStruct = "struct forward *lol;"
          fooStruct = "struct foo { int bar; int baz; };"
          clipseUnion = "union clipse { char pusha; char malice; };"
          staticInt = "static int notatypedef;"
          