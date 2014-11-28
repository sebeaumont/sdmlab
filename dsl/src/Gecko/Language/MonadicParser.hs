{-
   Copyright (c) 2014 Simon Beaumont. Cambridge, England.
   All Rights Reserved.

   Part of Gecko language analyzer.
   See: LICENCE for terms and conditions.
-}

module Gecko.Language.MonadicParser
       ( Parse
       , thenE
       , injectE
       , failE
       , catchE
       , injectP
       , thenP
       , failP
       , catchP
       ) where

-- parse error monad
data Parse a = OK a | Failed String deriving (Show)

-- bind
thenE :: Parse a -> (a -> Parse b) -> Parse b
m `thenE` k =
  case m of
   OK a -> k a
   Failed e -> Failed e

-- return :-)
injectE :: a -> Parse a
injectE = OK

-- fail
failE :: String -> Parse a
failE = Failed

-- catch combinator
catchE :: Parse a -> (String -> Parse a) -> Parse a
catchE m k =
  case m of
   OK a -> OK a
   Failed e -> k e


-- threaded lexer
-- what is this type?
type P a = String -> Parse a

-- bind
thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s ->
  case m s of
   OK a -> k a s
   Failed e -> Failed e

-- return by another name
injectP :: a -> P a
injectP a _ = OK a

failP :: String -> P a
failP err _ = Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k s =
  case m s of
   OK a -> OK a
   Failed e -> k e s
