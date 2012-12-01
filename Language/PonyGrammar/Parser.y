{
  module Language.PonyGrammar.Parser where
import Data.Char
}

%name parseGrammar
%tokentype { Token }
%error { parseError }

%token
  '::+' { TokenAdd }
  '::=' { TokenEq }
  gram { TokenGrammar }
  extends { TokenExt }
  '.' { TokenPeriod }
  '[' { TokenLSBracket }
  ']' { TokenRSBracket }
  '{' { TokenLBracket }
  '}' { TokenRBracket }
  '(' { TokenLParen }
  ')' { TokenRParen }
  identifier { TokenId $$ }
  
%%

Grammar : gram identifier '{' Prods '}' { Grammar $2 $4 }
        | gram identifier extends identifier '{' Prods '}' { GrammarAdd $2 $4 $6 }
                  
Prods   : Prod Prods { $1 : $2 }
        | { [] }

Prod    : identifier '::=' Expr '.' { Prod $1 $3 }
        | identifier '::+' Expr '.' { ProdAdd $1 $3 }

Expr    : Terms { Expr $1 }

Terms   : Term Terms { $1 : $2 }
        | { [] }

Term    : Factors { Term $1 }

Factors : Factor Factors { $1 : $2 }
        | { [] }

Factor  : identifier { Id $1 }
        | '[' Expr ']' { Optional $2 }
	| '(' Expr ')' { Paren $2 }
	| '{' Expr '}' { Many $2 }

{

parseError :: [Token] -> a
parseError ts = error ("Parse Error: " ++ (show ts))

data Grammar = Grammar Identifier [Production]
             | GrammarAdd Identifier Identifier [Production]
             deriving Show

data Production = Prod Identifier Expression
                | ProdAdd Identifier Expression
                deriving Show

data Expression = Expr [Term]
                deriving Show
                         
data Term = Term [Factor]
            deriving Show

data Factor = Id Identifier
            | Optional Expression
            | Paren Expression
            | Many Expression
            deriving Show

type Identifier = String

data Token = TokenAdd
           | TokenEq
	   | TokenGrammar
	   | TokenExt
	   | TokenPeriod
	   | TokenLSBracket
	   | TokenRSBracket
	   | TokenLBracket
	   | TokenRBracket
	   | TokenLParen
	   | TokenRParen
	   | TokenId String
           deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
      | isSpace c = lexer cs
      | isAlpha c = lexId (c:cs)
lexer ('.' : cs) = TokenPeriod : (lexer cs)
lexer ('[' : cs) = TokenLSBracket : (lexer cs)
lexer (']' : cs) = TokenRSBracket : (lexer cs)
lexer ('{' : cs) = TokenLBracket : (lexer cs)
lexer ('}' : cs) = TokenRBracket : (lexer cs)
lexer ('(' : cs) = TokenLParen : (lexer cs)
lexer (')' : cs) = TokenRParen : (lexer cs)
lexer (':' : ':' : '=' : cs) = TokenEq : (lexer cs)
lexer (':' : ':' : '+' : cs) = TokenAdd : (lexer cs)

lexId cs = case span isAlphaNum cs of
  ("Grammar", rest) -> TokenGrammar : (lexer rest)
  ("extends", rest) -> TokenExt : (lexer rest)
  (id, rest) -> (TokenId id) : (lexer rest)

parseGrammar' :: String -> Grammar
parseGrammar' = parseGrammar . lexer

}

