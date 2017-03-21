{- |
This is the module documentation for the Algebra
-}

module Database.SDM.Algebra where 

import Database.SDM.Internal.SDMLIB (SDMDataWord, SDMVectorIdx, sdmDataEls)
import qualified Data.Bits as B
import qualified Data.Set as Set
import qualified Data.List as List 


-- typeclass which generalises operations over vectors

class LinAlg a where
  add :: a -> a -> a
  --scale :: Num n => a -> n -> a
  --dotp..

-- vector type

data Vec = SVec [SDMDataWord]
         | EVec [SDMVectorIdx]
         deriving (Show)
         

xorv :: Vec -> Vec -> Vec
xorv (SVec a) (SVec b) = SVec $ zipWith B.xor a b
xorv (EVec a) (EVec b) = let us = Set.fromDistinctAscList $ List.nub $ List.sort a 
                             vs = Set.fromDistinctAscList $ List.nub $ List.sort b
                         in EVec $ Set.toList $ Set.difference us vs

andv :: Vec -> Vec -> Vec
andv (SVec a) (SVec b) = SVec $ zipWith (B..&.) a b
andv (EVec u) (EVec v) = let us = Set.fromDistinctAscList $ List.nub $ List.sort u 
                             vs = Set.fromDistinctAscList $ List.nub $ List.sort v
                         in EVec $ Set.toList $ Set.intersection us vs

orv :: Vec -> Vec -> Vec
orv (SVec a) (SVec b) = SVec $ zipWith (B..|.) a b
orv (EVec u) (EVec v) = EVec $ List.nub . List.sort $ u ++ v


popv :: Vec -> Int
popv (SVec a) = sum [B.popCount i | i <- a] 
popv (EVec u) = length u


zerov :: Vec
zerov = SVec $ replicate sdmDataEls 0


{-
wrotv :: SVec -> Int -> SVec
dot :: SVec -> SVec  
wedge ::
-}

instance LinAlg Vec where
  add (SVec v) (SVec u) = SVec $ zipWith (B..|.) u v
  add (EVec u) (EVec v) = EVec $ List.nub . List.sort $ u ++ v


popop :: (Vec -> Vec -> Vec) ->
         Vec ->
         Vec ->
         Int
popop f u v = popv $ f u v

{-
bitset :: SVec -> SDMDataWord -> SVec
bitset (SVec u) i = 
  let w = i `div` SDMDataEls
      b = i `rem` SDMDataEls
  in B..|. (u !! w) (bit b)
-}

{-
-- TODO operations on mixed types: e.g.  or :: EVec -> SVec -> SVec
-- we will want SVec as outputs for the search pattern
-- can't think of any use cases for EVec output yet


orsv :: EVec -> SVec -> SVec
orsv (EVec e) (SVec s) = SVec $ bitset e s where
  bitset i v = -- compute indexes and shifts as we do in c++ or something smarter?

-}
