{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

--import Control.Concurrent
import Control.Monad.IO.Class (liftIO)

import Database.SDM.Query.Parser
import Database.SDM.Algebra
import Database.SDM.Query.Eval
import Database.SDM.Query.IO
import Database.SDM.Query.AST

import System.Console.Haskeline
--import qualified Data.Text as T
import Data.List (intercalate)


main :: IO ()
main = do
  db <- openDB "/Volumes/Media/Data/medline/medline.sdm" (1024*1024*2048) (1024*1024*2048)
  case db of
    Left e -> putStrLn $ "Opening db: " ++ show e
    Right d -> runInputT defaultSettings (readEvalPrint d)


readEvalPrint :: SDMDatabase -> InputT IO ()
readEvalPrint db = do
  maybeLine <- getInputLine "> "
  case maybeLine of
    Nothing -> return () -- eof/null
    Just "quit" -> return ()
    Just line -> do
      case parseTopo line of
        Left e -> outputStrLn $ "** syntax: " ++ (show line) ++ (show e)
        Right t -> do
          top <- liftIO $ eval db t
          outputStrLn $ render top
      readEvalPrint db


-- hmm

render :: Either SDMStatus LevelSet -> String
render s = case s of
  Left e -> "OOPS: " ++ (show e)
  Right t -> intercalate "\n" $ [show x | x <- (fst t)] 


-- test_topo db = eval db (Topo "words" 0.5 0.5 10 (Or (State "words" "Sherlock") (State "words" "Watson")))
test_topo db = eval db (Topo "words" 0.5 0.5 10 (State "words" "tachycardia"))

term_test db s = eval db (Topo "words" 0.5 0.5 10 (State "words" s))


