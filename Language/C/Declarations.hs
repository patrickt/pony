module Language.C.Declarations where
  
  import Data.Maybe
  import Debug.Trace
  import Language.C.AST
  import Language.C.Declarators
  import Language.C.Expressions
  import Language.C.Lexer as L
  import Text.Parsec hiding (string)
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
    trace "Array" $ (return $ Array [] c)
  
  declaration = do
    typs <- many1 specifier
    -- TODO: nothing here requires that you actually have a type here
    -- e.g. "const static x;" parses correctly
    -- there should be a parsing failure if you don't specify a type qualifier
    ptrs <- many derived
    ident <- L.identifier
    arr <- optionMaybe array
    let dervs = if (arr == Nothing) then ptrs else ((fromJust arr): ptrs)
    L.semi
    return $ TopLevel typs (Named ident dervs) Nothing