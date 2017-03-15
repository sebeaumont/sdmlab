{- |
This is the module documentation for the Algebra
-}

module Database.SDM.Algebra where 

import Database.SDM.Internal.SDMLIB (SDMBitVector(SDMBitVector, toArray))
import qualified Data.Bits as B


-- appl Data.Bits operations over state or semantic vectors

xorv :: SDMBitVector -> SDMBitVector -> SDMBitVector
xorv a b = SDMBitVector $ zipWith B.xor (toArray a) (toArray b)

andv :: SDMBitVector -> SDMBitVector -> SDMBitVector
andv a b = SDMBitVector $ zipWith (B..&.) (toArray a) (toArray b)

orv :: SDMBitVector -> SDMBitVector -> SDMBitVector
orv a b = SDMBitVector $ zipWith (B..|.) (toArray a) (toArray b)

popv :: SDMBitVector -> Int
popv a = sum [B.popCount i | i <- toArray a] 

{-
wrotv :: SDMBitVector -> Int -> SDMBitVector
dot :: SDMBitVector -> SDMBitVector  
wedge ::
-}

popop :: (SDMBitVector -> SDMBitVector -> SDMBitVector) ->
         SDMBitVector ->
         SDMBitVector ->
         Int
popop f u v = popv $ f u v
