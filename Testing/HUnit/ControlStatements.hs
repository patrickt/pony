{-# LANGUAGE OverloadedStrings #-}
module Testing.HUnit.ControlStatements
    -- ( tests
    -- ) where
    where
  
  import Data.Generics.Fixplate
  import GHC.Exts
  import Language.C99.QuasiQuote
  import Language.Pony hiding (name)
  import qualified Data.ByteString.Char8 as B
  import Test.Framework
  import Test.Framework.Providers.HUnit
  import Test.Framework.TH
  import Test.HUnit hiding (Test)
  import Text.PrettyPrint.Free hiding (text)
  
  roundTrip :: ByteString -> Assertion
  roundTrip s = theory @=? practice where
    theory = pretty s
    practice = (para' evalPretty $ parseUnsafe statement s)
  
  
  -- A Body builds up not only test case data but also a name for the test case
  data Body = Body {name :: String, text :: B.ByteString}

  -- Monoid instance allows us to append both the test case data and the name
  instance Monoid Body where
    mempty = Body "" ""
    (Body n t) `mappend` (Body n' t') = Body (n ++ n') (t `B.append` t')
  
  -- Convenience function that just adds a prefix to the name and no test case data
  prefixName :: String -> Body -> Body
  prefixName n = mappend $ mempty {name = n}

  -- Convenience instance, represents strings as Bodys with no name information
  instance IsString Body where
    fromString a = mempty {text = B.pack a}
  
  assignBody = Body "Assign" "x = 5;"
  unaryBody  = Body "Unary"  "x++;"   
  callBody   = Body "Call"   "foo();"
  groupBody  = Body "Group"  [here|{
  x++;
}|]

  bodies = [groupBody, assignBody, unaryBody, callBody]
  
  -- These operate in the list monad and as such their types are all [Body]
  ifStmts     = do { b <- bodies; return $
                     prefixName "ifStmt"
                     "if (foo) " `mappend` b }
  ifElseStmts = do { b1 <- bodies; b2 <- bodies; return $
                     prefixName "ifElseStmt"
                     "if (foo) " `mappend` b1 `mappend` " else " `mappend` b2 }
  forStmts    = do { b <- bodies; return $
                     prefixName "forStmt"
                     "for(int i;i < 0;i++) " `mappend` b }
  whileStmts  = do { b <- bodies; return $
                     prefixName "whileStmt"
                     "while (foo) " `mappend` b }

  stmts = concat [ifStmts, ifElseStmts, forStmts, whileStmts]
            
  tests :: [Test]
  tests = [testGroup "Testing.HUnit.ControlStatements" [testCase n $ roundTrip t | Body n t <- stmts]]
              