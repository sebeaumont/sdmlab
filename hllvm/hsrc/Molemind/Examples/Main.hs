-- test main
module Main (main) where

import Molemind.Examples
import Molemind.Examples.RF

main :: IO ()
main = do
  putStrLn "Calling regular function..."
  print $ myfunction "tester"
  
  putStrLn "Calling primop dispatcher to construct record..."
  print $ getMessage 3
  print $ getMessage 2
  print $ getMessage 1
  
