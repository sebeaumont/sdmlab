--{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples  #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Molemind.Examples (Message(..), getMessage) where

import GHC.Int
import GHC.Prim
--import GHC.Ptr
import GHC.Word

foreign import prim "get_message_STG" getMessage# :: Word# -> (# Int#, Word#, Word#, Word#, Word#, Word# #)

-- 
data Message = Good { payload :: {-# UNPACK #-} !Word64 }
             | Bad { error :: {-# UNPACK #-} !Word64 }
             | Ugly { nah :: {-# UNPACK #-} !Int64 }
             deriving (Show)


-- this is test function
getMessage :: Word -> Message
getMessage (W# p) = case getMessage# p of
  (# 1#, _, _, _, _, _ #) -> Good (W64# 1957##)
  (# 2#, _, _, _, _, _ #) -> Bad (W64# 1984##)
  (# code, _, _, _, _, _ #) -> Ugly (I64# code)

