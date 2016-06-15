-- test main
module Main (main) where

import Molemind.Examples (getMessage, Message)

main :: IO ()
main = do
  putStrLn "Calling primop dispatcher to construct record..."
  print $ getMessage 1
  print $ getMessage 2
  print $ getMessage 3
  
