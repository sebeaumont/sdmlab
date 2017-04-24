{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Concurrent
import Data.Time
import Diagrams.Backend.Canvas
import Diagrams.Prelude
import Graphics.Blank hiding (rotate, scale, square, ( # ))

-- generates a diagram from time

clock :: UTCTime -> Diagram B
clock t = circle 0.35 # fc silver # lwG 0 
          <> bigHand # f 12 h
          <> littleHand # f 60 m
          <> secondHand # f 60 (q s)
          <> circle 1  # fc black # lwG 0
          <> circle 11 # lwG 1 # lc white # fc lightsteelblue # pad 1.1
  where
    s = realToFrac $ utctDayTime t :: Double
    m = s / 60
    h = m / 60
    
    bigHand    = (0 ^& (-1.5)) ~~ (0 ^& 7.5) # lwG 0.5
    littleHand = (0 ^& (-2))   ~~ (0 ^& 9.5) # lwG 0.2
    secondHand = (0 ^& (-1.5)) ~~ (0 ^& 10.0) # lwG 0.1
    
    f n v = rotate (- v / n @@ turn)
    -- | local function to round double to nearest unit
    q :: Double -> Double
    q = roundn 0 

-- | Round to n places
-- roundn :: (Fractional b, RealFrac a, Integral n) => n -> a -> b
roundn :: Int -> Double -> Double 
roundn n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

main :: IO ()
main = blankCanvas 3000 $ \context -> loop context

loop :: DeviceContext -> IO a
loop context = do
  tn <- getCurrentTime
  send context $ renderDia Canvas (CanvasOptions (mkWidth 200))
                                  (clock tn)
  threadDelay 1000000 -- nice animation of second hand
  loop context
