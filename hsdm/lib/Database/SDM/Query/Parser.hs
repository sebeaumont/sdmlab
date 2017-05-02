{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE ExistentialQuantification #-}

-- | Module for DSL syntax  

module Database.SDM.Query.Parser where

--import Control.Applicative
import Control.Monad (void)

import qualified Database.SDM.Query.AST as AST (Expr(..)) 
import Database.SDM.Algebra

import Data.Functor.Identity

import Text.Megaparsec 
import Text.Megaparsec.String
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Prim as P
import qualified Text.Megaparsec.Lexer as L

-- | Define what whitespace means for this language

whitespace :: Parser ()
whitespace = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "--"
        blockCmnt = L.skipBlockComment "{-" "-}"

-- | Lexeme consumes trailing whitespace 

lexeme :: ParsecT Dec String Identity a -> ParsecT Dec String Identity a
lexeme = L.lexeme whitespace

double :: ParsecT Dec String Identity Double
double = lexeme L.float

integer :: ParsecT Dec String Identity Integer
integer = lexeme L.integer

-- | Symbols

sym :: String -> ParsecT Dec String Identity String
sym = L.symbol whitespace

parens :: ParsecT Dec String Identity a -> ParsecT Dec String Identity a
parens = between (sym "(") (sym ")")

stringLiteral :: (Token s ~ Char, P.MonadParsec e s m) => m [Char]
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

-- | Operators

ors :: ParsecT Dec String Identity String
ors = sym "|"

ands :: ParsecT Dec String Identity String
ands = sym "&"

nots :: ParsecT Dec String Identity String
nots = sym "~"

operators :: [[Operator Parser (AST.Expr Vec)]]
operators = [[Prefix (AST.Not <$ nots)],
             [InfixL (AST.And <$ ands)],
             [InfixL (AST.Or <$ ors)]]

-- | Identifiers

identifier :: ParsecT Dec String Identity [Char]
identifier = stringLiteral <|> (lexeme . try) (many alphaNumChar) 


term :: ParsecT Dec String Identity (AST.Expr Vec)
term = parens vexpr <|> AST.State <$> identifier <*> identifier

-- | Vector valued expression

vexpr :: Parser (AST.Expr Vec)
vexpr = makeExprParser term operators 

-- parse a string to an exprssion... 
parseExpr :: String -> Either (ParseError (Token String) Dec) (AST.Expr Vec)
parseExpr s = parse vexpr "expression" s
