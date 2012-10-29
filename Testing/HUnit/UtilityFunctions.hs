module Testing.HUnit.UtilityFunctions 
  ( tests ) where
  
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Test.HUnit hiding (Test)
  import Language.Pony
  import Language.C99.Miscellany
  
  isTypedef :: Bool -> String -> Test
  isTypedef b code = testCase code $ assertEqual "declarationIsTypedef is wrong" b $ declarationIsTypedef d where
    (CTranslationUnit [ExternDecl d]) = parseUnsafe preprocessedC code
  
  isComposite :: Bool -> String -> Test
  isComposite b code = testCase code $ assertEqual "declarationIsComposite is wrong" b $ declarationIsComposite d where
    (CTranslationUnit [ExternDecl d]) = parseUnsafe preprocessedC code
  
  hasFields b code = testCase code $ assertEqual "declarationHasFields is wrong" b $ declarationHasFields d where
    (CTranslationUnit [ExternDecl d]) = parseUnsafe preprocessedC code
  
  tests :: [Test]
  tests = [ isTypedef True "typedef int foo;"
          , isTypedef True "typedef struct foo_s { int blah; int baz; } foo_t;"
          , isTypedef True "typedef int (*clever)(float monkeys);"
          , isTypedef False staticInt
          , isComposite True fooStruct
          , isComposite True clipseUnion
          , isComposite False staticInt
          , hasFields True fooStruct
          , hasFields True clipseUnion
          , hasFields False staticInt
          , hasFields False "struct forward *lol;"
          ]
        where
          fooStruct = "struct foo { int bar; int baz; };"
          clipseUnion = "union clipse { char pusha; char malice; };"
          staticInt = "static int notatypedef;"
          