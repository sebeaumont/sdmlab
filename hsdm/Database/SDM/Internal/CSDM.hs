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

newtype SDMDatabase = SDMDatabase (Ptr SDMDatabase) deriving (Storable)


foreign import ccall unsafe "dsmlib.h sdm_open_database"
  c_sdm_open_db :: CString -> CInt -> CInt -> Ptr SDMDatabase -> IO CInt

foreign import ccall unsafe "dsmlib.h sdm_close_database"
  c_sdm_close_db :: Ptr SDMDatabase -> IO CInt


sdm_open_db :: String -> Int -> Int -> IO (SDMDatabase, Integer)
sdm_open_db file size maxsize =
  withCString file $ \str -> 
                       alloca $ \dbptr -> do
    i <- c_sdm_open_db str (fromIntegral size) (fromIntegral maxsize) dbptr
    d <- peek dbptr
    return (d, toInteger i)
