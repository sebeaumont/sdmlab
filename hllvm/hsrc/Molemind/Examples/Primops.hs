{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples  #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Molemind.Examples (Message(..), getMessage) where

import GHC.Int
import GHC.Prim
import GHC.Ptr
import GHC.Word

foreign import prim "get_message_STG" getMessage# :: Word# -> (# Int#, Word# #)

-- 
data Message = Good { payload :: {-# UNPACK #-} !Word64 }
             | Bad { error :: {-# UNPACK #-} !Word64 }
             | Ugly { part :: {-# UNPACK #-} !Word64 }
             deriving (Show)


-- this is test function
getMessage :: Word -> Message
getMessage (W# p) = case getMessage# p of
  (# 1#, stuff #) -> Good (W64# 1957##)
  (# 2#, error #) -> Bad (W64# 1984##)
  (# part, _ #) -> Ugly (W64# 1999##)

