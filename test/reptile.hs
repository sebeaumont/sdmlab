
module Main where

import Convert
import System.Environment(getArgs)
import Data.Int
import LLVM.Core
import LLVM.ExecutionEngine

main :: IO ()
main = do
  initializeNativeTarget
  args <- getArgs
  m <- readBitcodeFromFile (args !! 0)
  mp <- createModuleProviderForExistingModule m
  funcs <- getModuleValues m

  print funcs
  
  let fnn :: Function (Int32 -> IO Int32)
      Just fnn = lookup "fib" funcs >>= castModuleValue
      
  iofn <- runEngineAccess $ do
    addModuleProvider mp
    --generateFunction fnn
    getPointerToFunction fnn
    
  let fn = convert iofn
  fn (read (args !! 1)) >>= print
  -- let fn = unsafePurify $ convert iofn
  -- print (fn $ read (args !! 1))

