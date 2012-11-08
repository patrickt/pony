{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}

module Language.C99.Lexer 
  ( identifier
  , whiteSpace
  , reserved
  , operator
  , reservedOp
  , reservedOp'
  , charLiteral
  , stringLiteral
  , natural
  , float
  , symbol
  , lexeme
  , braces
  , brackets
  , parens
  , semi
  , comma
  , colon
  , dot
  , arrow
  , semiSep
  , semiSep1
  , commaSep
  , commaSep1
  ) 
  
  where
  
  import Control.Applicative
  import Data.ByteString (ByteString)
  import Data.Functor.Identity (Identity)
  import Numeric (readOct, readHex)
  import Text.Parsec hiding ((<|>), many)
  import Text.Parsec.Language hiding (LanguageDef)
  import qualified Text.Parsec.Token as T
  
  type LanguageDef st = GenLanguageDef ByteString st Identity
  
  -- Currently trigraphs, digraphs, _Bool, _Complex, and _Imaginary are unsupported.
  ponyCDef :: LanguageDef st
  ponyCDef = T.LanguageDef 
    { T.reservedOpNames = ["->", "++", "--", "&", "*", "+", "-", "~", "!", "/", "%", "<<", ">>", 
                          "/", "%", "<<", ">>", "<", ">", "<=", ">=", "==", "!=", "^", "|", "&&", 
                          "?", ":", ";", "...", "=", "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", 
                          "&=", "^=", "|=", ",", "#", "##", "[", "]", "?:"]    
    , T.reservedNames = ["asm", "auto", "break", "case", "char", "const", "continue", "default", "do", 
                        "double", "else", "enum", "extern", "float", "for", "goto", "if", "inline", 
                        "int", "long", "register", "restrict", "return", "short", "signed", "sizeof", 
                        "static", "struct", "switch", "typedef", "union", "unsigned", "void", 
                        "volatile", "while", "...", "__asm", "__attribute__", "__inline", "__inline__",
                        "__typeof__", "__builtin_va_arg", "__int128_t", "__uint128_t"]
    , T.identStart = letter <|> char '_'
    , T.identLetter = alphaNum <|> oneOf "_'"
    , T.commentStart = "/*"
    , T.commentEnd = "*/"
    , T.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , T.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , T.commentLine = "#"
    , T.caseSensitive = True
    , T.nestedComments = True
    }

  cOctal = char '0' >>
           many1 octDigit >>= 
           return . fst . head . readOct           
  cHex = char '0' >> oneOf "xX" >>
         many1 hexDigit >>=
         return . fst . head . readHex
  
  lexer = T.makeTokenParser ponyCDef
  {-# INLINE lexer #-}

  whiteSpace = T.whiteSpace lexer
  identifier = T.identifier lexer
  reserved = T.reserved lexer
  operator = T.operator lexer
  reservedOp = T.reservedOp lexer
  reservedOp' n = reservedOp n >> return n
  charLiteral = T.charLiteral lexer
  stringLiteral = T.stringLiteral lexer
  natural = try cHex <|> try cOctal <|> (T.decimal lexer)
  float = try floating <|> suffixed
    where
      floating       = assemble <$> (try float1 <|> float2) <*> optional' exponentPart <*> optional' suffix 
      suffixed       = assemble <$> some digit <*> exponentPart <*> optional' suffix
      float1         = assemble <$> many digit <*> string "." <*> some digit
      float2         = (++) <$> string "." <*> some digit
      exponentPart   = assemble <$> (single $ oneOf "eE") <*> sign <*> some digit
      sign           = optional' (single $ oneOf "-+")
      assemble x y z = x ++ y ++ z
      suffix         = single $ oneOf "flFL"
      optional'      = option ""
      single         = fmap (:[]) 
    
  symbol = T.symbol lexer
  lexeme = T.lexeme lexer
  parens = T.parens lexer
  braces = T.braces lexer
  -- angles = T.angles lexer -- (unused)
  brackets = T.brackets lexer
  semi = T.semi lexer
  comma = T.comma lexer
  colon = T.colon lexer
  dot = T.dot lexer
  arrow :: ParsecT ByteString u Identity ByteString
  arrow = reservedOp "->" >> return "->"
  semiSep = T.semiSep lexer
  semiSep1 = T.semiSep1 lexer
  commaSep = T.commaSep lexer
  commaSep1 = T.commaSep1 lexer
