module Language.C.Lexer 
  ( identifier
  , whiteSpace
  , reserved
  , operator
  , reservedOp
  , charLiteral
  , stringLiteral
  , natural
  , integer
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
  
  import Data.Char (isSpace)
  import Text.Parsec
  import Text.Parsec.Language
  import qualified Text.Parsec.Token as T
  
  -- Currently trigraphs, digraphs, _Bool, _Complex, and _Imaginary are unsupported.
  ponyCDef = javaStyle 
    { T.reservedOpNames = ["->", "++", "--", "&", "*", "+", "-", "~", "!", "/", "%", "<<", ">>", 
                          "/", "%", "<<", ">>", "<", ">", "<=", ">=", "==", "!=", "^", "|", "&&", 
                          "?", ":", ";", "...", "=", "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", 
                          "&=", "^=", "|=", ",", "#", "##", "[", "]"]    
    , T.reservedNames = ["asm", "auto", "break", "case", "char", "const", "continue", "default", "do", 
                        "double", "else", "enum", "extern", "float", "for", "goto", "if", "inline", 
                        "int", "long", "register", "restrict", "return", "short", "signed", "sizeof", 
                        "static", "struct", "switch", "typedef", "union", "unsigned", "void", 
                        "volatile", "while", "...", "__asm", "__attribute__", "__inline", "__inline__",
                        "__typeof__", "__builtin_va_arg" ]
    , T.identStart = letter <|> char '_'
    -- Since the C preprocessor removes comments, there is no need to 
    -- have facilities for //, /* and */. Tee-hee.
    , T.commentLine = "#"
    }
  
  lexer = T.makeTokenParser ponyCDef

  whiteSpace = T.whiteSpace lexer
  identifier = T.identifier lexer
  reserved = T.reserved lexer
  operator = T.operator lexer
  reservedOp = T.reservedOp lexer
  charLiteral = T.charLiteral lexer
  stringLiteral = T.stringLiteral lexer
  natural = T.natural lexer
  integer = T.integer lexer
  float = T.float lexer
  naturalOrFloat = T.naturalOrFloat lexer
  decimal = T.decimal lexer
  hexadecimal = T.hexadecimal lexer
  octal = T.octal lexer
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