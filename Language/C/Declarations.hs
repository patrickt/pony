module Language.C.Declarations
  ( declaration
  )
where
  
  import Data.Maybe
  import Debug.Trace
  import Language.C.AST
  import Language.C.Declarators
  import Language.C.Expressions
  import Language.C.Lexer as L
  import Language.C.Parser
  
  into = flip fmap
  
  specifier :: Parser Specifier
  specifier = choice 
    [ typeSpecifier `into` TSpec
    , typeQualifier `into` TQual
    , storageSpecifier `into` SSpec
    ]
    
  derived :: Parser DerivedDeclarator
  derived = choice [ pointer ]
    
  pointer :: Parser DerivedDeclarator
  pointer = do
    char '*'
    L.whiteSpace
    quals <- many typeQualifier
    
    return $ Pointer quals
  
  array :: Parser DerivedDeclarator
  array = do 
    c <- L.brackets constant
    return $ Array [] c
  
  
  -- TODO: enforce that the binary operator has to be an assignment
  -- BUG: array declarations are broken again
  declaration :: Parser CDeclaration
  declaration = do
    typs <- many1 specifier
    -- TODO: nothing here requires that you actually have a type here
    -- e.g. "const static x;" parses correctly
    -- there should be a parsing failure if you don't specify a type qualifier
    ptrs <- many derived
    expr <- expression
    let result = peruseExpression typs ptrs expr
    L.semi
    result
    where
      peruseExpression typs ptrs (Identifier ident) = return $ TopLevel typs (Named ident ptrs) Nothing
      peruseExpression typs ptrs (BinaryOp eq (Identifier lhs) rhs) = return $ TopLevel typs (Named lhs ptrs) (Just rhs)
      peruseExpression typs ptrs (Call (Identifier name) rhs) = return $ (TopLevel typs (Named name (Function : ptrs)) Nothing)
      peruseExpression _ _ _ = fail "invalid declarator or bug (the latter is more likely)"
