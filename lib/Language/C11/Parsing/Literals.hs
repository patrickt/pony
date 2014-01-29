module Language.C11.Parsing.Literals
  ( parseChar
  , parseFloat
  , parseInteger
  ) where
  
  import Data.Comp
  import Data.Comp.Ops
  import qualified Data.Text as T
  import qualified Data.ByteString.Char8 as B
  import Language.C11.Syntax
  import Language.C99.Parser hiding (char)
  import StringTable.Atom
  
  import qualified Language.C99.Lexer as L 

  parseInteger :: Parser (Term IntLit)
  parseInteger = choice 
    [ try $ iIntLit <$> L.hex <*> pure 16 <*> suffix
    , try $ iIntLit <$> L.octal <*> pure 8 <*> suffix
    , iIntLit <$> L.decimal <*> pure 10 <*> suffix
    ]
    
  suffix :: Parser (Maybe ByteString)
  suffix = do 
    chars <- many $ oneOf "uUlL"
    L.whiteSpace
    return $ if (chars == []) 
      then Nothing
      else Just $ B.pack chars 
    
  parseChar :: Parser (Term ChrLit)
  parseChar = iChrLit <$> L.charLiteral
  
  parseFloat :: Parser (Term FltLit)
  parseFloat = iFltLit <$> L.float <*> pure 10 <*> suffix