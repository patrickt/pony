module Language.C11.Parsing.Expressions 
  ( parseIdent
  , parseStringLiteral
  , parseExpression
  , ExpressionSig
  , module Language.C11.Parsing.Literals
  ) where 
  
  import Control.Arrow
  import Language.Pony.Overture
  import Language.C11.Syntax
  import Language.C99.Parser hiding (char)
  import Language.C11.Parsing.Literals
  import Data.Comp
  import Data.ByteString.Char8 (pack)
  import qualified Language.C99.Lexer as L 
  import Control.Lens
  
  -- can we use that new GHC extension to write a synonym for (Functor f, Foo :<: f, Bar :<: f) => f ?
  type ExpressionSig = Literal :+: Ident :+: Expr :+: Operator
  
  parseIdent :: Parser (Term Ident)
  parseIdent = iIdent <$> Name <$> pack <$> L.identifier
  
  parseStringLiteral :: Parser (Term Literal)
  parseStringLiteral = iStrLit <$> pack <$> concat <$> L.stringLiteral `sepBy1` L.whiteSpace <?> "string literal"
  
  parseExpression :: Parser (Term ExpressionSig)
  parseExpression = prefixExpression
  
  parsePrimaryExpression :: Parser (Term ExpressionSig)
  parsePrimaryExpression = choice [ deepInject <$> parseIdent
                                  , deepInject <$> parseLiteral
                                  , deepInject <$> parseStringLiteral 
                                  , iParen <$> L.parens parseExpression
                                  ]
                                  
                                  
  postfixExpression :: Parser (Term ExpressionSig)
  postfixExpression = do
    subject <- parsePrimaryExpression
    postfixes <- many postfixExpressionBuilder
    return $ foldl (>>>) id postfixes subject
  
  prefixExpressionBuilder :: Parser (Term ExpressionSig -> Term ExpressionSig)
  prefixExpressionBuilder = choice
    [ iUnary <$> (iDec <$ L.reservedOp "--")
    , iUnary <$> (iInc <$ L.reservedOp "++")
    -- , iCast  <$> try (L.parens typeName)
    , iUnary <$> (deepInject <$> parseUnaryOperator)
    ]
  
  
  -- TODO: the use of 'symbol' here is very dubious
  parseUnaryOperator :: Parser (Term Operator)
  parseUnaryOperator = choice 
    [ iRef <$ L.symbol "&"
    , iDeref <$ L.symbol "*"
    , iPos <$ L.symbol "+"
    , iNeg <$ L.symbol "-"
    , (iBitwise Neg) <$ L.symbol "~"
    , iNot <$ L.symbol "!"
    , iSizeOf <$ (try $ L.symbol "sizeof")
    ]
  
  postfixExpressionBuilder :: Parser (Term ExpressionSig -> Term ExpressionSig)
  postfixExpressionBuilder = choice 
    [ iIndex <$$> L.brackets parseExpression
    , iCall  <$$> L.parens (L.commaSep parseExpression)
    , iDot   <$$> L.dot *> fmap deepInject parseIdent
    , iArrow <$$> L.arrow *> fmap deepInject parseIdent
    , iUnary <$$> (iPostInc <$ L.reservedOp "++")
    , iUnary <$$> (iPostDec <$ L.reservedOp "--")
    ]
    
  prefixExpression :: Parser (Term ExpressionSig)
  prefixExpression = foldl (<<<) id <$> many prefixExpressionBuilder <*> postfixExpression
  
  constantExpression :: Parser (Term ExpressionSig)
  constantExpression = E.buildExpressionParser table prefixExpression