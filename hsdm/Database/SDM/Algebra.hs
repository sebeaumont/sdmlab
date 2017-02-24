{- |
This is the module documentation for the Algebra
-}

module Database.SDM.Algebra where 

import Database.SDM.Internal.SDMLIB (SDMBitVector(SDMBitVector, toArray))
import qualified Data.Bits as B

type SVec = SDMBitVector

-- appl Data.Bits operations over Vector

xorv :: SVec -> SVec -> SVec
xorv a b = SDMBitVector $ zipWith B.xor (toArray a) (toArray b)

andv :: SVec -> SVec -> SVec
andv a b = SDMBitVector $ zipWith (B..&.) (toArray a) (toArray b)

orv :: SVec -> SVec -> SVec
orv a b = SDMBitVector $ zipWith (B..|.) (toArray a) (toArray b)

popv :: SVec -> Int
popv a = sum [B.popCount i | i <- toArray a] 

