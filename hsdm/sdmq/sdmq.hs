{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Concurrent

import Database.SDM.Algebra
import Database.SDM.Query.Eval
import Database.SDM.Query.IO
import Database.SDM.Query.AST

import Diagrams.Backend.Canvas
import Diagrams.Prelude

import Graphics.Blank (DeviceContext, blankCanvas, send, clearCanvas)
import qualified Graphics.SVGFonts as SF
import qualified Data.Text as T

-- | SVG font text with origin at left end of baseline
--text' :: (Read n, Renderable (Path V2 n) b, RealFloat n, Data.Typeable.Internal.Typeable n) => n -> String -> QDiagram b V2 n Any

text' d s = (SF.textSVG_ (SF.TextOpts SF.lin SF.INSIDE_H SF.KERN False d d) s) # lw none

-- | SVG font text with origin at centre of text

text'' d s = (strokeP $ SF.textSVG' (SF.TextOpts SF.lin SF.INSIDE_H SF.KERN False d d) s) # lw none

-- test data
type NodeInfo = (String, Double, Double)

node :: NodeInfo -> Diagram B
node (s, p, d) = text' p s # fc blue # light # opacity (sqrt (1.0 - d))


graph :: [NodeInfo] -> Diagram B
graph l = atPoints (trailVertices $ regPoly (length l) 2.5) (map node l) # pad 1.5


-- | Actual data types we want to visualise

node' :: SDMPoint -> Diagram B
node' p = text' (metric p) (T.unpack $ symbol p) # fc blue # light # opacity (sqrt (1.0 - density p))

node'' :: SDMPoint -> Diagram B
node'' p = text' 1.0 (T.unpack $ symbol p) # fc blue # light

graph' :: LevelSet -> Diagram B
graph' l = atPoints (trailVertices $ regPoly (length $ fst l) 2.5) (map node' (fst l)) # pad 1.5



-- | Round to n places
-- roundn :: (Fractional b, RealFrac a, Integral n) => n -> a -> b
roundn :: Int -> Double -> Double 
roundn n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)


main :: IO ()
main = do
  db <- openDB "/Users/seb/Data/ash.sdm" (1024*1024*700) (1024*1024*700)
  case db of
    Left e -> putStrLn $ show e
    Right d -> blankCanvas 3000 $ \context -> loop context d


loop :: DeviceContext -> SDMDatabase -> IO ()
loop context db = do
  top <- test_topo db
  case top of
    Left e -> putStrLn $ show e
    Right t -> do
      putStrLn $ show t
      send context (clearCanvas >> renderDia Canvas (CanvasOptions (mkWidth 400)) (graph' t))
      threadDelay (10*1000000) -- uSec
      loop context db



test_topo db = eval db (Topo "words" 0.5 0.5 10 (Or (State "words" "Sherlock") (State "words" "Watson")))


