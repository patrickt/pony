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
  
  tests :: [Test]
  tests = [ isTypedef True "typedef int foo;"
          , isTypedef True "typedef struct foo_s { int blah; int baz; } foo_t;"
          , isTypedef True "typedef int (*clever)(float monkeys);"
          , isTypedef False "static int notatypedef;"]