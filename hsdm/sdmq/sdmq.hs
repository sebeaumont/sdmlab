{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

--import Control.Concurrent
import Control.Monad.IO.Class (liftIO)

import Database.SDM
import Database.SDM.Query.Parser
import Database.SDM.Query.Eval
import Database.SDM.Query.IO

import System.Console.Haskeline
--import System.Console.Haskeline.Completion

import qualified Data.Text as T
import Data.List (intercalate)


-- | completion fn

wordComplete :: Monad m => (String -> String -> m [Completion]) -> CompletionFunc m
wordComplete f = completeWordWithPrev (Just '\\') " \t\n" f

-- |   
completeTerm :: SDMDatabase -> [Char] -> String -> IO [Completion]
completeTerm db l w = do
  let tryspace = last . words $ reverse l
  -- see if tryspace can be found
  sv <- getSpace db tryspace
  case sv of
    Left e -> return []
    Right s -> do
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
  
-- | "Let no man enter who is ignorant of geometry"

main :: IO ()
main = do
  -- TODO command line 
  db <- openDB "/Volumes/Media/Data/medline/medline.sdm" (1024*1024*2048) (1024*1024*2048)
  case db of
    Left e -> putStrLn $ "Opening db: " ++ show e
    -- Right d -> runInputT defaultSettings (readEvalPrint d)
    Right d -> runInputT Settings { complete = wordComplete (completeTerm d),
                                    historyFile = Just ".halhist",
                                    autoAddHistory = True } (readEvalPrint d)


-- | Every language needs a repl!
-- This one takes a `SDMDatabase` in order to evaluate queries. This should be
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
          top <- liftIO $ eval db t
          outputStrLn $ render top
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


