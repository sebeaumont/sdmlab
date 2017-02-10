{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- DSM library bindings
module Database.SDM.Internal.CSDM where

import Foreign
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import Foreign.C.Types
import Foreign.C.String

-- could auto dealloc this
newtype SDMDatabase = SDMDatabase (Ptr SDMDatabase) deriving (Storable)


foreign import ccall unsafe "dsmlib.h sdm_open_database"
  c_sdm_open_db :: CString -> CInt -> CInt -> Ptr SDMDatabase -> IO CInt

foreign import ccall unsafe "dsmlib.h sdm_close_database"
  c_sdm_close_db :: SDMDatabase -> IO CInt

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
newtype SDMSpace = SDMSpace (Ptr SDMSpace) deriving (Storable)

foreign import ccall unsafe "dsmlib.h sdm_ensure_space"
  c_sdm_ensure_space :: SDMDatabase -> CString -> Ptr SDMSpace -> IO CInt

sdm_ensure_space :: SDMDatabase -> String -> IO (SDMSpace, Integer)
sdm_ensure_space db spacename =
  withCString spacename $ \str -> 
                            alloca $ \ptr -> do
    i <- c_sdm_ensure_space db str ptr
    s <- peek ptr
    return (s, toInteger i)

foreign import ccall unsafe "dsmlib.h sdm_get_space"
  c_sdm_get_space :: SDMDatabase -> CString -> Ptr SDMSpace -> IO CInt

{- TBC -}

--
-- vector in space
--
newtype SDMVector = SDMVector (Ptr SDMVector) deriving (Storable)
-- now we need to define a vector type to receive actual vector data, n.b. the
-- vector_t in the c api is just an apaque pointer to the vector object. 

foreign import ccall unsafe "dsmlib.h sdm_ensure_symbol"
  c_sdm_ensure_symbol :: SDMSpace -> CString -> Ptr SDMVector -> IO CInt

sdm_ensure_symbol :: SDMSpace -> String -> IO (SDMVector, Integer)
sdm_ensure_symbol space symbolname =
  withCString symbolname $ \str ->
                             alloca $ \ptr -> do
    i <- c_sdm_ensure_symbol space str ptr
    s <- peek ptr
    return (s, toInteger i)


-- data SDMVectordata = SDMVectordata [a]

-- sdm_get_vectordata ::  
