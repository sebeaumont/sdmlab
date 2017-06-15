{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE ExistentialQuantification #-}

-- | Module for Parsing Strings/Text into AST

module Database.SDM.Query.Parser (parseExpr, parseTopo, parseTerms) where
--import Control.Applicative
import Control.Monad (void)

import Database.SDM
-- generalisation + implementation of vector space operations
import Database.SDM.Algebra 

-- the AST defnitions for mini-query language
import qualified Database.SDM.Query.AST as AST (Expr(..), Stmt(..)) 

-- using megaparsec parser combinator library
import Data.Functor.Identity
import Text.Megaparsec 
import Text.Megaparsec.String
--import Text.Megaparsec.Char
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

-- | Operators... really vector valued functions

ors :: ParsecT Dec String Identity String
ors = sym "|"

ands :: ParsecT Dec String Identity String
ands = sym "&"

nots :: ParsecT Dec String Identity String
nots = sym "~"


-- table of operations for expression parser

operators :: [[Operator Parser (AST.Expr Vec)]]
operators = [[Prefix (AST.Not <$ nots)],
             [InfixL (AST.And <$ ands)],
             [InfixL (AST.Or <$ ors)]]


-- | Identifiers

identifier :: ParsecT Dec String Identity [Char]
identifier = stringLiteral <|> (lexeme . try) (many alphaNumChar) 

-- AST.Expr Vec valued parsers...

-- | term parser

term :: ParsecT Dec String Identity (AST.Expr Vec)
term = parens vexpr <|> (try basis <|> state)


state :: ParsecT Dec String Identity (AST.Expr Vec)
state = AST.State <$> identifier <*> identifier


basis :: ParsecT Dec String Identity (AST.Expr Vec)
basis = AST.Elem <$> identifier <* sym "^" <*> identifier


-- | vector expression parser

vexpr :: Parser (AST.Expr Vec)
vexpr = makeExprParser term operators 


-- | parse a string to an vector valued expression... 

parseExpr :: String -> Either (ParseError (Token String) Dec) (AST.Expr Vec)
parseExpr s = parse vexpr "expression" s


-- Statements/command parsers

-- | Parser for a LevelSet

topoP :: Parser (AST.Expr LevelSet) 
topoP = do
  _ <- sym "topo"
  s <- identifier
  p <- double
  d <- double
  n <- integer 
  x <- vexpr
  return $ AST.Topo s p d (fromIntegral n) x

parseTopo :: String -> Either (ParseError (Token String) Dec) (AST.Expr LevelSet)
parseTopo s = parse topoP "topology" s

termsP :: Parser (AST.Expr TermMatch)
termsP = do
  _ <- sym "terms"
  s <- identifier
  t <- identifier
  n <- integer 
  return $ AST.Terms s t (fromIntegral n)

parseTerms :: String -> Either (ParseError (Token String) Dec) (AST.Expr TermMatch)
parseTerms s = parse termsP "terms" s


-- messing about... with dynamic dispatch

data L = Halt

evalL :: Parser String
evalL = do
  _ <- sym "@"
  f <- identifier
  return f

parseL s = parse evalL "repl" s

-- wont typecheck as we cant combine parsers of different types
-- commandP = termsP -- try (topoP <|> termsP)


{-
parse' :: String -> Either (ParseError (Token String) Dec) (AST.Expr TermMatch)
parse' s = parse commandP "command" s
-}
