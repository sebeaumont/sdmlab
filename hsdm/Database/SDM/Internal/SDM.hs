{-# LANGUAGE ForeignFunctionInterface #-}
-- DSM library bindings
module Database.SDM.Internal.CSDM where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.C.String

data SDMDatabase = SDMDatabase

foreign import ccall unsafe "dsmlib.h sdm_open_database"
sdmOpenDB :: CString -> CUInt -> CUInt -> Ptr SDMDatabase -> IO CInt

foreign import ccall unsafe "dsmlib.h sdm_close_database"
sdmCloseDB :: Ptr SDMDatabase -> IO CInt
