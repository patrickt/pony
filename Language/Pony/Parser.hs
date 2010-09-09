{-# LANGUAGE UnicodeSyntax, RankNTypes #-}

module Language.Pony.Parser where
  
  import Control.Monad.Identity
  import Language.Pony.Lexer
  import Text.Parsec
  import qualified Text.Parsec.Token as Token
  import Text.Parsec.Language
  
  data Language = Language String deriving (Show, Eq)
  
  type Parser t = ∀ a. ParsecT String a Identity t
  
  languageDeclarator :: Parser Language
  languageDeclarator = do
    string "%% Language: "
    lang ← manyTill anyChar (try (string "\n"))
    return $ Language lang
  