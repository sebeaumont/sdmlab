{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

#include <sdmlib.h>

{#context lib = "sdm" #}

module Database.SDM.Internal.SDMLIB where

import System.IO.Unsafe (unsafeDupablePerformIO)

import Foreign
--import Foreign.Ptr
--import Foreign.ForeignPtr
import Foreign.C
--import Foreign.C.Types
--import Foreign.C.String

import qualified Data.ByteString as BS
import Data.Text
import Data.Text.Encoding


-- CString utf-8 Text utils 

tdecode :: CString -> IO Text
tdecode cstr = do
  bytestr <- BS.packCString cstr
  return (decodeUtf8 bytestr)

tencode :: Text -> (CString -> IO a) -> IO a
tencode text cont =
  BS.useAsCString (encodeUtf8 text) cont

  
-- 
-- Generally functions in the sdm c api return a status_t and return objects
-- via opaque typed pointers.
--

type SDMCard = {#type card_t#}

type SDMStatus = {#type status_t #}

is_error :: SDMStatus -> Bool
is_error = (<0)

------------------------------------
-- IO wrapper for FFI actions
------------------------------------

-- newtype SDMDB a = SDMDB { runSDMDB :: IO a } deriving (Functor, Applicative, Monad)

------------------------------------
-- Opaque pointers to Foreign data
------------------------------------

--
-- SDM Database
--

newtype SDMDatabase = SDMDatabase (Ptr SDMDatabase) deriving (Storable, Show)

foreign import ccall unsafe "sdm_database"
  c_sdm_open_db :: CString -> CInt -> CInt -> Ptr SDMDatabase -> IO SDMStatus

foreign import ccall unsafe "sdm_database_close"
  c_sdm_close_db :: SDMDatabase -> IO SDMStatus

-- SDMDatabase factory requires a filename and some size bounds
--
sdm_database :: String -> Int -> Int -> IO (SDMDatabase, SDMStatus)
sdm_database file size maxsize =
  withCString file $ \str -> 
                       alloca $ \dbptr -> do
    i <- c_sdm_open_db str (fromIntegral size) (fromIntegral maxsize) dbptr
    d <- peek dbptr
    return $ (d,  i)
    
-- this could be managed by finalser
sdm_database_close :: SDMDatabase -> IO ()
sdm_database_close db = c_sdm_close_db db >> return ()  

--
-- space the final frontier
--

newtype SDMSpace = SDMSpace (Ptr SDMSpace) deriving (Storable, Show)

foreign import ccall unsafe "sdm_database_ensure_space"
  c_sdm_ensure_space :: SDMDatabase -> CString -> Ptr SDMSpace -> IO SDMStatus

sdm_database_ensure_space :: SDMDatabase -> String -> IO (SDMSpace, SDMStatus)
sdm_database_ensure_space db spacename =
  withCString spacename $ \str -> 
                            alloca $ \ptr -> do
    i <- c_sdm_ensure_space db str ptr
    s <- peek ptr
    return (s,  i)

foreign import ccall unsafe "sdm_database_get_space"
  c_sdm_get_space :: SDMDatabase -> CString -> Ptr SDMSpace -> IO SDMStatus

{- TBC -}

--
-- symbols
-- 

newtype SDMSymbol = SDMSymbol (Ptr SDMSymbol) deriving (Storable, Show)

foreign import ccall unsafe "sdm_database_ensure_symbol"
  c_sdm_ensure_symbol :: SDMDatabase -> SDMSpace -> CString -> Ptr SDMSymbol -> IO SDMStatus

sdm_database_ensure_symbol :: SDMDatabase -> SDMSpace -> String -> IO (SDMSymbol, SDMStatus)
sdm_database_ensure_symbol db space symbolname =
  withCString symbolname $ \str ->
                             alloca $ \ptr -> do
       i <- c_sdm_ensure_symbol db space str ptr
       s <- peek ptr
       return (s, i)


foreign import ccall unsafe "sdm_space_get_symbol"
  c_sdm_get_symbol :: SDMSpace -> CString -> Ptr SDMSymbol -> IO SDMStatus

sdm_space_get_symbol :: SDMSpace -> String -> IO (SDMSymbol, SDMStatus)
sdm_space_get_symbol space symbolname =
  withCString symbolname $ \str ->
                             alloca $ \ptr -> do
       i <- c_sdm_get_symbol space str ptr
       s <- peek ptr
       return (s, i)


--
-- vectors in space
--

-- vector_t in the c api is just an opaque pointer to the vector object. 
newtype SDMVector = SDMVector (Ptr SDMVector) deriving (Storable, Show)


foreign import ccall unsafe "sdm_space_get_vector"
  c_sdm_get_vector :: SDMSpace -> CString -> Ptr SDMVector -> IO SDMStatus

sdm_space_get_vector :: SDMSpace -> String -> IO (SDMVector, SDMStatus)
sdm_space_get_vector space symbolname =
  withCString symbolname $ \str ->
                             alloca $ \ptr -> do
       i <- c_sdm_get_vector space str ptr
       s <- peek ptr
       return (s, i)


foreign import ccall unsafe "sdm_space_get_symbol_vector"
  c_sdm_space_get_symbol_vector :: SDMSpace -> SDMSymbol -> Ptr SDMVector -> IO SDMStatus

sdm_space_get_symbol_vector :: SDMSpace -> SDMSymbol -> IO (SDMVector, SDMStatus)
sdm_space_get_symbol_vector sp sy = alloca $ \ptr -> do
  i <- c_sdm_space_get_symbol_vector sp sy ptr
  v <- peek ptr
  return (v, i)
  

--
-- vectorpayload
--

type SDMDataWord = {#type vectordata_t #}

sdmDataEls :: SDMVectorIdx
sdmDataEls = {#const SDM_VECTOR_ELEMS#}

foreign import ccall unsafe "sdm_vector_load"
  c_sdm_load_vector :: SDMVector -> Ptr SDMDataWord -> IO SDMStatus

sdm_vector_load :: SDMVector -> IO ([SDMDataWord], SDMStatus)
sdm_vector_load v = do
  -- here allocate Haskell managed memory (ForeignPtr) with default
  -- finalisers (free) for vector data to be written
  aptr <- mallocForeignPtrArray {#const SDM_VECTOR_ELEMS#} :: IO (ForeignPtr SDMDataWord)
  withForeignPtr aptr $ \ptr -> do
    i <- c_sdm_load_vector v ptr
    a <- peekArray {#const SDM_VECTOR_ELEMS#} ptr
    return (a, i)



-- retrieve basic type for sparse index
type SDMVectorIdx = {#type basis_t #}

foreign import ccall unsafe "sdm_symbol_get_basis"
  c_sdm_get_basis :: SDMSymbol -> Ptr SDMVectorIdx -> IO SDMStatus

-- this is pure for a given symbol
sdm_symbol_get_basis :: SDMSymbol -> [SDMVectorIdx]
sdm_symbol_get_basis sym =
  unsafeDupablePerformIO $ do
  aptr <- mallocForeignPtrArray {#const SDM_VECTOR_BASIS_SIZE#} :: IO (ForeignPtr SDMVectorIdx)
  withForeignPtr aptr $ \ptr -> do
     _ <- c_sdm_get_basis sym ptr
     a <- peekArray {#const SDM_VECTOR_BASIS_SIZE#} ptr
     return a


-- Defintion of a point marshalled from C point_t

data SDMPoint = SDMPoint { symbol  :: !Text,
                           metric  :: !Double,
                           density :: !Double
                         } deriving (Show)

instance Storable SDMPoint where
  sizeOf _ = {#sizeof point_t#}
  alignment _ = {#alignof point_t#}
  peek ptr = do
    s <- {#get point_t.symbol#} ptr >>= tdecode
    -- s <- peekCString c
    m <- {#get point_t.metric#} ptr >>= return . realToFrac
    d <- {#get point_t.density#} ptr >>= return . realToFrac
    return $ SDMPoint s m d
  poke _ (SDMPoint _ _ _) = undefined


--
-- the shape of things
--

foreign import ccall unsafe "sdm_space_get_topology"
  c_sdm_space_get_topology :: SDMSpace
                           -> Ptr SDMDataWord
                           -> CDouble
                           -> CDouble
                           -> CUInt
                           -> Ptr SDMPoint
                           -> IO SDMCard

-- | Warning this could truncate its input: however it is only used to size the
-- allocation of data we copy over FFI which is unlikely to overflow an Int.
--
toInt :: SDMCard -> Int
toInt = fromInteger . toInteger


-- | get list of nearest points from a space given the parameters:
--
sdm_space_get_topology :: SDMSpace        -- ^ space to search
                       -> Double          -- ^ lower bound of similarity
                       -> Double          -- ^ upper bound of density
                       -> Int             -- ^ upper bound of cardinality of computed point set
                       -> [SDMDataWord]   -- ^ pattern vector
                       -> IO ([SDMPoint], SDMCard)
sdm_space_get_topology s m d n v = do
  -- max card allocation (n) may be wasteful in some highly constrained cases.
  withArray v $ \vp -> do
    aptr <- mallocForeignPtrArray n :: IO (ForeignPtr SDMPoint)
    withForeignPtr aptr $ \ptr -> do
      i <- c_sdm_space_get_topology s vp (realToFrac m) (realToFrac d) (fromIntegral n) ptr
      a <- peekArray (toInt i) ptr
      return (a, i)


-- prefix search -- with newstyle JSON serializers

newtype SDMBuffer = SDMBuffer (Ptr SDMBuffer) deriving (Storable, Show)


foreign import ccall unsafe "sdm_space_serialize_symbols"
  c_sdm_space_serialize_symbols :: SDMSpace -> CString -> CUInt -> Ptr SDMBuffer -> IO SDMCard


foreign import ccall unsafe "sdm_buffer_data"
  c_sdm_buffer_data :: SDMBuffer -> IO CString

foreign import ccall unsafe "sdm_buffer_free"
  c_sdm_buffer_free :: SDMBuffer -> IO ()


-- | get serialized symbol buffer for (upto n) symbols matching prefix
sdm_space_serialize_symbols :: SDMSpace -> String -> Int -> IO (BS.ByteString, SDMCard)
sdm_space_serialize_symbols space prefix n = 
  withCString prefix $ \str ->
                         alloca $ \ptr -> do
    -- gets the size in bytes of the buffer
    i <- c_sdm_space_serialize_symbols space str (fromIntegral n) ptr
    tp <- peek ptr
    cs <- c_sdm_buffer_data tp
    -- length to ensure we are safe on the string copy
    s <- BS.packCStringLen (cs, toInt i)
    -- no leaks please!
    c_sdm_buffer_free tp
    return (s, i)

