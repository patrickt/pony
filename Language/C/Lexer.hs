{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.C.Lexer 
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
  
  import Control.Applicative (some)
  import Numeric (readOct, readHex)
  import Text.Parsec
  import Text.Parsec.Language
  import qualified Text.Parsec.Token as T
  
  -- Currently trigraphs, digraphs, _Bool, _Complex, and _Imaginary are unsupported.
  ponyCDef :: LanguageDef st
  ponyCDef = javaStyle 
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
    -- Since the C preprocessor removes comments, there is no need to 
    -- have facilities for //, /* and */. Tee-hee.
    , T.commentLine = "#"
    }

  cOctal = char '0' >>
           many1 octDigit >>= 
           return . fst . head . readOct           
  cHex = char '0' >> oneOf "xX" >>
         many1 hexDigit >>=
         return . fst . head . readHex
  
  lexer = T.makeTokenParser ponyCDef

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
      floating = do
        frac <- fractConstant
        expPart <- option [] exponentPart
        suf <- option [] suffix
        return (frac ++ expPart ++ suf)
      suffixed = do
        digits <- some digit
        expPart <- exponentPart
        suf <- option [] suffix
        return (digits ++ expPart ++ suf)
      float1 = do
        leading <- many digit
        char '.'
        trailing <- some digit
        return (leading ++ "." ++ trailing)
      float2 = do
        leading <- some digit
        char '.'
        return $ leading ++ "."
      digitSequence = some digit
      fractConstant = try float1 <|> float2
      exponentPart = do
        e <- oneOf "eE" >>= \x -> return [x]
        sign <- option [] (string "-" <|> string "+")
        digits <- some digit
        return (e ++ sign ++ digits)
      suffix = oneOf "flFL" >>= \x -> return [x]
    
  symbol = T.symbol lexer
  lexeme = T.lexeme lexer
  parens = T.parens lexer
  braces = T.braces lexer
  angles = T.angles lexer
  brackets = T.brackets lexer
  semi = T.semi lexer
  comma = T.comma lexer
  colon = T.colon lexer
  dot = T.dot lexer
  arrow = reservedOp "->" >> return "->"
  semiSep = T.semiSep lexer
  semiSep1 = T.semiSep1 lexer
  commaSep = T.commaSep lexer
  commaSep1 = T.commaSep1 lexer