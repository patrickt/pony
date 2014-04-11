module Language.C11.Parsing.Literals
  ( parseLiteral
  ) where
  
  import Data.Comp
  import qualified Data.ByteString.Char8 as B
  import Language.C11.Syntax
  import Language.C99.Parser hiding (char)
  import qualified Language.C99.Lexer as L 
  
  parseSuffix :: Parser (Maybe ByteString)
  parseSuffix = do 
    chars <- many $ oneOf "uUlL"
    L.whiteSpace
    return $ if (chars == []) 
      then Nothing
      else Just $ B.pack chars
  
  parseInteger :: Parser (Term Literal)
  parseInteger = choice 
    [ try $ iIntLit <$> L.hex     <*> pure 16 <*> parseSuffix
    , try $ iIntLit <$> L.octal   <*> pure 8  <*> parseSuffix
    ,       iIntLit <$> L.decimal <*> pure 10 <*> parseSuffix
    ]
    
  parseChar :: Parser (Term Literal)
  parseChar = iChrLit <$> L.charLiteral
  
  -- parseFloat is buggy as heck: reading 1.1f fails in Prelude.read
  parseFloat :: Parser (Term Literal)
  parseFloat = try $ iFltLit <$> L.float <*> pure 10 <*> parseSuffix
  
  parseLiteral :: Parser (Term Literal)
  parseLiteral = choice [ parseFloat
                        , parseInteger
                        , parseChar
                        ]