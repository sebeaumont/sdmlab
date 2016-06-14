{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples  #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Molemind.Examples (Message(...), getMessage) where

import GHC.Int
import GHC.Prim
import GHC.Ptr
import GHC.Word

foreign import prim "get_message_STG" getMessage# :: Word# -> (# ... #)


data Message = Good | Bad | Ugly

getMessage :: Word -> Message
getMessage (W# p) = case getMessage# p of  
