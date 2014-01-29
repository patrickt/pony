module Language.C11.Parsing.Literals
  ( parseChar
  , parseFloat
  , parseInteger
  , parseLiteral
  , suffix
  ) where
  
  import Data.Comp
  import Data.Comp.Ops
  import qualified Data.Text as T
  import qualified Data.ByteString.Char8 as B
  import Language.C11.Syntax
  import Language.C99.Parser hiding (char)
  import StringTable.Atom
  
  import qualified Language.C99.Lexer as L 

  parseInteger :: Parser (Const IntLit)
  parseInteger = choice 
    [ try $ IntLit <$> L.hex <*> pure 16 <*> suffix
    , try $ IntLit <$> L.octal <*> pure 8 <*> suffix
    , IntLit <$> L.decimal <*> pure 10 <*> suffix
    ]
    
  suffix :: Parser (Maybe ByteString)
  suffix = do 
    chars <- many $ oneOf "uUlL"
    L.whiteSpace
    return $ if (chars == []) 
      then Nothing
      else Just $ B.pack chars 
    
  parseChar :: Parser (Const ChrLit)
  parseChar = ChrLit <$> L.charLiteral
  
  parseFloat :: Parser (Const FltLit)
  parseFloat = try $ FltLit <$> L.float <*> pure 10 <*> suffix
  
  parseLiteral :: Parser (Term Constant)
  parseLiteral = choice [ injectConst <$> parseFloat
                        , injectConst <$> parseInteger
                        , injectConst <$> parseChar]