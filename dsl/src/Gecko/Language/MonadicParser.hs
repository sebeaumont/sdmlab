
module Gecko.Language.MonadicParser
       ( Parse
       , thenE
       , injectE
       , failE
       , catchE
       , injectP
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
type P a = String -> Parse a

-- bind
thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s ->
  case m s of
   OK a -> k a s
   Failed e -> Failed e

-- return
injectP :: a -> P a
injectP a = \s -> OK a

failP :: String -> P a
failP err = \s -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s ->
  case m s of
   OK a -> OK a
   Failed e -> k e s

