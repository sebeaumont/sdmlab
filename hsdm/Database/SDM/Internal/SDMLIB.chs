{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

#include <sdmlib.h>

{#context lib = "sdm" #}

module Database.SDM.Internal.SDMLIB where

import Foreign
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import Foreign.C.Types
import Foreign.C.String

--
-- All functions in the sdm c api return a status_t and an opaque reference via
-- an output argument pointer.
--
type SDMStatus = {#type status_t #}

is_error :: SDMStatus -> Bool
is_error = (<0)

--
-- SDM Database
--

newtype SDMDatabase = SDMDatabase (Ptr SDMDatabase) deriving (Storable, Show)

foreign import ccall unsafe "dsmlib.h sdm_open_database"
  c_sdm_open_db :: CString -> CInt -> CInt -> Ptr SDMDatabase -> IO SDMStatus

foreign import ccall unsafe "dsmlib.h sdm_close_database"
  c_sdm_close_db :: SDMDatabase -> IO SDMStatus

-- SDMDatabase factory requires a filename and some size bounds
--
sdm_open_db :: String -> Int -> Int -> IO (SDMDatabase, Integer)
sdm_open_db file size maxsize =
  withCString file $ \str -> 
                       alloca $ \dbptr -> do
    i <- c_sdm_open_db str (fromIntegral size) (fromIntegral maxsize) dbptr
    d <- peek dbptr
    return (d, toInteger i)
    
-- this could be managed by finalser
sdm_close_db :: SDMDatabase -> IO ()
sdm_close_db db = c_sdm_close_db db >> return ()  

--
-- space the final frontier
--
newtype SDMSpace = SDMSpace (Ptr SDMSpace) deriving (Storable, Show)

foreign import ccall unsafe "dsmlib.h sdm_ensure_space"
  c_sdm_ensure_space :: SDMDatabase -> CString -> Ptr SDMSpace -> IO SDMStatus

sdm_ensure_space :: SDMDatabase -> String -> IO (SDMSpace, Integer)
sdm_ensure_space db spacename =
  withCString spacename $ \str -> 
                            alloca $ \ptr -> do
    i <- c_sdm_ensure_space db str ptr
    s <- peek ptr
    return (s, toInteger i)

foreign import ccall unsafe "dsmlib.h sdm_get_space"
  c_sdm_get_space :: SDMDatabase -> CString -> Ptr SDMSpace -> IO SDMStatus

{- TBC -}

--
-- symbols
-- 
newtype SDMSymbol = SDMSymbol (Ptr SDMSymbol) deriving (Storable, Show)

foreign import ccall unsafe "dsmlib.h sdm_ensure_symbol"
  c_sdm_ensure_symbol :: SDMDatabase -> SDMSpace -> CString -> Ptr SDMSymbol -> IO SDMStatus

sdm_ensure_symbol :: SDMDatabase -> SDMSpace -> String -> IO (SDMSymbol, Integer)
sdm_ensure_symbol db space symbolname =
  withCString symbolname $ \str ->
                             alloca $ \ptr -> do
       i <- c_sdm_ensure_symbol db space str ptr
       s <- peek ptr
       return (s, toInteger i)


foreign import ccall unsafe "dsmlib.h sdm_get_symbol"
  c_sdm_get_symbol :: SDMSpace -> CString -> Ptr SDMSymbol -> IO SDMStatus

sdm_get_symbol :: SDMSpace -> String -> IO (SDMSymbol, Integer)
sdm_get_symbol space symbolname =
  withCString symbolname $ \str ->
                             alloca $ \ptr -> do
       i <- c_sdm_get_symbol space str ptr
       s <- peek ptr
       return (s, toInteger i)

--
-- vector in space
--

newtype SDMVector = SDMVector (Ptr SDMVector) deriving (Storable, Show)
-- now we need to define a vector type to receive actual vector data, n.b. the
-- vector_t in the c api is just an opaque pointer to the vector object. 

foreign import ccall unsafe "dsmlib.h sdm_get_vector"
  c_sdm_get_vector :: SDMSpace -> CString -> Ptr SDMVector -> IO SDMStatus

sdm_get_vector :: SDMSpace -> String -> IO (SDMVector, Integer)
sdm_get_vector space symbolname =
  withCString symbolname $ \str ->
                             alloca $ \ptr -> do
       i <- c_sdm_get_vector space str ptr
       s <- peek ptr
       return (s, toInteger i)

--
-- we allocate Haskell managed memory for vectordata
--

type SDMVecElem = {#type vectordata_t #}

foreign import ccall unsafe "dsmlib.h sdm_load_vector"
  c_sdm_load_vector :: SDMVector -> Ptr SDMVecElem -> IO SDMStatus

sdm_load_vector :: SDMVector -> IO ([SDMVecElem], Integer)
sdm_load_vector v = do
  aptr <- mallocForeignPtrArray0 {#const SDM_VECTOR_ELEMS#} :: IO (ForeignPtr SDMVecElem)
  withForeignPtr aptr $ \ptr -> do
    i <- c_sdm_load_vector v ptr
    a <- peekArray {#const SDM_VECTOR_ELEMS#} ptr
    return (a, toInteger i)
