{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Main where

import System.Environment(getArgs)
import System.Console.CmdArgs
import System.CPUTime
import Control.Monad (replicateM)

import Text.Printf
--import Criterion.Main
--import Criterion.Config

import Data.Int
import LLVM.Core
import LLVM.ExecutionEngine
import Convert

data TestTarget = TestTarget { iterations :: Int
                             , bitcode :: String
                             , function :: String
                             , fnarg :: Int32
                             } deriving (Show, Data, Typeable)
                                        
defaultTestTarget = TestTarget { iterations = 1000
                               , bitcode = "test.bc"
                               , function = "testfn"
                               , fnarg = 10
                               }
  
main :: IO ()
main = do
  initializeNativeTarget
  opts <- cmdArgs defaultTestTarget

  -- llvm load and resolve function
  m <- readBitcodeFromFile (bitcode opts)
  mp <- createModuleProviderForExistingModule m
  funcs <- getModuleValues m

  print funcs
  print (function opts)
  
  let fnn :: Function (Int32 -> IO Int32)
      Just fnn = lookup (function opts) funcs >>= castModuleValue
      
  iofn <- runEngineAccess $ do
    addModuleProvider mp
    --generateFunction fnn
    getPointerToFunction fnn

  printf "%s:%s [..%d]\n" (bitcode opts) (function opts) (iterations opts)
  let fn = convert iofn
  start <- getCPUTime
  results <- replicateM (iterations opts) (fn (fnarg opts)) 
  end <- getCPUTime

  let diff = (fromIntegral (end - start) / 10^12)
      total = sum results

  printf "%s:%s=%d (%0.6fs)\n" (bitcode opts) (function opts) total (diff :: Double)
  


