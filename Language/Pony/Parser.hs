{-# LANGUAGE UnicodeSyntax, RankNTypes #-}

module Language.Pony.Parser where
  
  import Control.Monad.Identity
  import Language.Pony.Lexer
  import Text.Parsec
  import qualified Text.Parsec.Token as Token
  import Text.Parsec.Language
  
  data Language = Language String deriving (Show, Eq)
  
  type Parser t = ∀ a. ParsecT String a Identity t
  
  preamble :: Parser String
  preamble = string "%% Language: "
  
  languageDeclarator :: Parser Language
  languageDeclarator = preamble >> anyChar `manyTill` newline >>= return . Language
  
  pony :: Language -> Parser [String]
  pony a = case (runParser languageDeclarator () "console" a) of 
      (Right _) → ponyC
      (Left _) → parserFail "Pony only supports PonyC at this point."
  
  whitespace :: Parser ()
  whitespace = Token.whiteSpace lexer
  
  ponyC :: Parser [String]
  ponyC = anything `sepEndBy` whitespace
  
  anything :: Parser String
  anything = many1 anyChar
  