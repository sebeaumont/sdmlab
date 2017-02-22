{- |
This is the module documentation for the Algebra
-}

module Database.SDM.Algebra where

import Database.SDM.Internal.SDMLIB (SDMBitVector(SDMBitVector, toArray))
import qualified Data.Bits as B

--type Vec = SDMBitVector

-- appl Data.Bits operations over Vector

xorv :: SDMBitVector -> SDMBitVector -> SDMBitVector
xorv a b = SDMBitVector $ zipWith B.xor (toArray a) (toArray b)

andv :: SDMBitVector -> SDMBitVector -> SDMBitVector
andv a b = SDMBitVector $ zipWith (B..&.) (toArray a) (toArray b)

orv :: SDMBitVector -> SDMBitVector -> SDMBitVector
orv a b = SDMBitVector $ zipWith (B..|.) (toArray a) (toArray b)

popv :: SDMBitVector -> Int
popv a = sum [B.popCount i | i <- toArray a] 

