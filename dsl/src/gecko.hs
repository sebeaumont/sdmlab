{-# LANGUAGE OverloadedStrings #-}
module Main where


import Gecko.Language.Analyzer (parser)

main :: IO ()
main = putStrLn "there are about 2000 species of gecko in 52 genera..."
       -- >> parser
