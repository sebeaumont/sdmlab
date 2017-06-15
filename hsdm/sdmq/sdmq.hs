{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

--import Control.Concurrent
import Control.Monad.IO.Class (liftIO)

import Database.SDM
import Database.SDM.Query.Parser
import Database.SDM.Query.Eval
import Database.SDM.Query.IO

import System.Console.CmdArgs (cmdArgs, Data, Typeable)
import System.Console.Haskeline
--import System.Console.Haskeline.Completion

import qualified Data.Text as T
import Data.List (intercalate)

-- safe at last
maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast l = Just $ last l

-- | toy repl to show how this stuff might work... 

-- | completion fn

wordComplete :: Monad m => (String -> String -> m [Completion]) -> CompletionFunc m
wordComplete f = completeWordWithPrev (Just '\\') " \t\n" f

-- somehing a bit applicative might be more readable than this ;-)
-- TODO be nice if we could complete on spacenames
completeTerm :: SDMDatabase -> [Char] -> String -> IO [Completion]
completeTerm db l w = do
  let tryspace = maybeLast . words $ reverse l
  -- see if tryspace can be found XX this way too hopeful as it should
  case tryspace of
    Nothing -> return []
    Just spacename -> do
      sv <- getSpace db spacename
      case sv of
        Left e -> return []
        Right s -> do
          -- get all matching terms or at least the unsigned representation of -1 terms - ahem
          tv <- getTerms s w (-1) 
          case tv of
            Nothing -> return []
            Just t -> do
              return $ toCompletion (termMatch t)


-- | terms to completions

toCompletion :: TermMatch -> [Completion]
toCompletion m = [Completion (T.unpack $ name t) (T.unpack $ name t) True | t <- terms m]


--completionFn :: CompletionFunc IO
--completionFn = wordComplete completeTerm

-- Program command line options

data SDMQ = SDMQ { database :: String,
                   size :: Int,
                   maxsize :: Int } deriving (Show, Data, Typeable)

defaultOptions :: SDMQ
defaultOptions = SDMQ { database = "/Volumes/Media/Data/medline/medline.sdm",
                        size = (1024*1024*2048),
                        maxsize = (1024*1024*2048)}
  
-- | "Let no man enter who is ignorant of geometry..."

main :: IO ()
main = do
  -- get command line args
  args <- cmdArgs defaultOptions
  db <- openDB (database args) (size args) (maxsize args) 
  case db of
    Left e -> putStrLn $ "opening db: " ++ show args ++ " error: " ++ show e
    -- Right d -> runInputT defaultSettings (readEvalPrint d)
    Right d -> runInputT Settings { complete = wordComplete (completeTerm d),
                                    historyFile = Just ".halhist",
                                    autoAddHistory = True } (readEvalPrint d)


-- | Every language needs a repl!
-- This one takes a `SDMDatabase` in order to evaluate expressions. This could be
-- a more abstract type.

readEvalPrint :: SDMDatabase -> InputT IO ()
readEvalPrint db = do
  maybeLine <- getInputLine "> "
  case maybeLine of
    Nothing -> return () -- eof/null
    Just "quit" -> return ()
    Just "" -> readEvalPrint db
    -- TODO work grammar out top down.
    Just line -> do
      case parseTopo line of
        Left e -> outputStrLn $ "** syntax: " ++ (show line) ++ "\n" ++ (show e)
        Right t -> do
          outputStrLn $ show t           -- tracing
          top <- liftIO $ eval db t
          outputStrLn $ render top       -- render
          -- TODO serialize query expression to message queue for viz
      readEvalPrint db


-- | Display is like Show typeclass for `render`ing to `String`

class Display a where
  render :: a -> String

-- instances to render results

instance Display (Either SDMStatus LevelSet) where
  render s = case s of
    Left e -> "** error: " ++ (show e)
    Right t -> render t

instance Display LevelSet where
  render l = render $ fst l

instance Display [SDMPoint] where
  render l = intercalate "\n" $ map render l

instance Display SDMPoint where
  render p = show (roundn 4 (metric p)) ++ "\t" ++ T.unpack (symbol p) ++ "\t" ++ show (roundn 3 (density p))
  

-- Round to n places
-- roundn :: (Fractional b, RealFrac a, Integral n) => n -> a -> b
roundn :: Int -> Double -> Double 
roundn n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)


-- test_topo db = eval db (Topo "words" 0.5 0.5 10 (Or (State "words" "Sherlock") (State "words" "Watson")))
-- test_topo db = eval db (Topo "words" 0.5 0.5 10 (State "words" "tachycardia"))
-- term_test db s = eval db (Topo "words" 0.5 0.5 10 (State "words" s))


