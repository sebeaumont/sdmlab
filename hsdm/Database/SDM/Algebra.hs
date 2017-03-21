{- |
This is the module documentation for the Algebra
-}

module Database.SDM.Algebra where 

import Database.SDM.Internal.SDMLIB (SDMDataWord, SDMVectorIdx, sdmDataEls)
import qualified Data.Bits as B
import qualified Data.Set as Set
import qualified Data.List as List 


-- typeclass which generalises operations over vectors

class Vec a where
  add :: a -> a -> a
  --scale :: Num n => a -> n -> a
  

-- operations over state or semantic vectors

newtype  SVec =  SVec [SDMDataWord] deriving (Show)

xorv :: SVec -> SVec -> SVec
xorv (SVec a) (SVec b) = SVec $ zipWith B.xor a b

andv :: SVec -> SVec -> SVec
andv (SVec a) (SVec b) = SVec $ zipWith (B..&.) a b

orv :: SVec -> SVec -> SVec
orv (SVec a) (SVec b) = SVec $ zipWith (B..|.) a b


instance Vec SVec where
  add = xorv


popv :: SVec -> Int
popv (SVec a) = sum [B.popCount i | i <- a] 

zerov :: SVec
zerov = SVec $ replicate sdmDataEls 0


{-
wrotv :: SVec -> Int -> SVec
dot :: SVec -> SVec  
wedge ::
-}

popop :: (SVec -> SVec -> SVec) ->
         SVec ->
         SVec ->
         Int
popop f u v = popv $ f u v

{-
bitset :: SVec -> SDMDataWord -> SVec
bitset (SVec u) i = 
  let w = i `div` SDMDataEls
      b = i `rem` SDMDataEls
  in B..|. (u !! w) (bit b)
-}

--  
-- Sparse algebra over elemental vectors...
--

newtype EVec = EVec [SDMVectorIdx] deriving (Show)

ors :: EVec -> EVec -> EVec
ors (EVec u) (EVec v) = EVec $ List.nub . List.sort $ u ++ v

xors :: EVec -> EVec -> EVec
xors (EVec u) (EVec v) =
  let us = Set.fromDistinctAscList $ List.nub $ List.sort u 
      vs = Set.fromDistinctAscList $ List.nub $ List.sort v
  in EVec $ Set.toList $ Set.difference us vs

ands :: EVec -> EVec -> EVec
ands (EVec u) (EVec v) = 
  let us = Set.fromDistinctAscList $ List.nub $ List.sort u 
      vs = Set.fromDistinctAscList $ List.nub $ List.sort v
  in EVec $ Set.toList $ Set.intersection us vs

pops :: EVec -> Int
pops (EVec u) = length u

instance Vec EVec where
  add = xors
  
{-
-- TODO operations on mixed types: e.g.  or :: EVec -> SVec -> SVec
-- we will want SVec as outputs for the search pattern
-- can't think of any use cases for EVec output yet


orsv :: EVec -> SVec -> SVec
orsv (EVec e) (SVec s) = SVec $ bitset e s where
  bitset i v = -- compute indexes and shifts as we do in c++ or something smarter?




-}
