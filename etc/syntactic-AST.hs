import Data.Text

data E a
data S
data T

type Symbol = Text

data LitDom a where
  CInteger :: Integer -> LitDom (Full (E Integer))
  CFloat   :: Integer -> LitDom (Full (E String))
  CChar    :: Char    -> LitDom (Full (E Char))
  CString  :: Text    -> LitDom (Full (E Text))
  -- chose E rather than L because all literals are valid expressions!! mods?!
  
data ExprDom a where
  Comma :: ??? -> ??? -> ExprDom (Full (E ???)) -- ??? = expressions or literals
  Ident :: Text       -> ExprDom (Full (E Symbol))
  Index :: ExprDom (E :-> E :-> Full   (E ???)) -- what does it contain? i have no clue
  Call  :: ExprDom (E :-> E [] -> Full (E ???)) -- E [] ???? oh god, how do we do lists? do we need typelevel lists like HList?
  Cast  :: ExprDom (T :-> E a  -> Full (E ???)) -- ideally we want this to have type corresponding to the type value contained
                                                -- in the type this expression is casting too. how can we do that, though?
                                                
  Postfix :: Symbol -> ExprDom (E a :-> Full (E a))     -- i think this is right
  Prefix :: Symbol -> ExprDom (E a :-> Full (E a))
  Binary :: Symbol -> ExprDom (E a :-> E b -> ???)
  Ternary :: ExprDom (E a :-> E b :-> E c :-> Full (E ???))
  SizeOf  :: ExprDom (T a :-> Full (E ???))
  Builtin :: Symbol -> ExprDom (Full (E Symbol))
  Paren :: Stmtdom (E a :-> Full (E a))

data StmtDom a where
  Break :: StmtDom (Full S)
  Case, Switch :: StmtDom (E a :-> S :-> Full S)
  Compound :: StmtDom ??? -- again, don't know how to do lists
  Continue :: StmtDom (Full S)
  Default :: StmtDom (S :-> Full S)
  DoWhile :: StmtDom (S :-> E a :-> Full S)
  Empty   :: StmtDom (Full S)
  For     :: StmtDom (??? :-> Full S) -- should be a list of expressions or declarations a: [a] -> Statement -> Statement
  Goto    :: StmtDom (E a :-> Full S)
  IfThenElse :: StmtDom (E a :-> S :-> ??? :-> Full S) --- how do we express (Maybe Statement)
  Labeled :: StmtDom (E a :-> S :-> Full S)
  Return  :: StmtDom (??? :-> S) -- again, not sure how to do Maybe here
  While :: StmtDom (E a :-> S :-> Full S)
  
  