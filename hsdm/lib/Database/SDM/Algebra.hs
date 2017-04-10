{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{- |
This is the module documentation for the Algebra module
-}

module Database.SDM.Algebra
  (Vec(..),
   xorv,
   andv,
   orv,
   popv,
   popop,
   toDense,
   Vector,
   add,
   scale,
   InnerProduct,
   inner,
   mul,
   ExteriorProduct,
   wedge,
   GeometricProduct,
   gproduct,
   Metric,
   distance) where


import Database.SDM.Internal.SDMLIB (SDMDataWord, SDMVectorIdx, sdmDataEls)
import qualified Data.Bits as B
import qualified Data.Set as Set
import qualified Data.List as List 


-- | typeclasses which generalise operations over vectors

-- should really be over two classes or more: the vector and scalar types for
-- the algebra, we may also need a type family of k-vectors or k-blades

class Vector a where
  add :: a -> a -> a
  scale :: Num k => k -> a -> a
  scale _ v = v

class InnerProduct a n | a -> n where
  inner :: Num n => a -> a -> n
  mul :: a -> a -> a
  
class ExteriorProduct a b where
  wedge :: a -> a -> b


{- N.B. a->b fundep is a lie! -}
class (ExteriorProduct a b, InnerProduct a n) => GeometricProduct a n b | a -> n, a -> b where
  gproduct :: a -> a -> a


class Metric a d where
  distance :: Num d => a -> a -> d


-- | concrete vector type with sparse and dense representations

data Vec = SVec [SDMDataWord]
         | EVec [SDMVectorIdx]
         deriving (Show)
         
-- | zero data for dense vector

zerow :: [SDMDataWord]
zerow = replicate (fromIntegral sdmDataEls) 0

-- | map index into word and bit address

wordbit:: SDMVectorIdx -> (SDMVectorIdx, SDMVectorIdx)
wordbit i = (i `div` sdmDataEls, i `rem` sdmDataEls)

-- | Convert sparse vector to dense format

toDense :: Vec -> Vec
toDense (EVec u) = SVec $ zipWith (bitset $ Set.fromDistinctAscList u) [0..] zerow where
  bitset :: Set.Set SDMVectorIdx -> SDMVectorIdx -> SDMDataWord -> SDMDataWord
  bitset idx i w | Set.member i idx =  w `B.setBit` fromIntegral (snd $ wordbit i)
                 | otherwise = w
toDense u@(SVec _) = u


-- TODO toSparse :: Vec -> Vec
-- then policy?
  
popv :: Vec -> Int
popv (SVec a) = sum [B.popCount i | i <- a] 
popv (EVec u) = length u


-- | Apply binary operation and popcount result

popop :: (Vec -> Vec -> Vec) ->
         Vec ->
         Vec ->
         Int
popop f u v = popv $ f u v


-- | Or to add two vectors

orv :: Vec -> Vec -> Vec
orv (SVec a) (SVec b) = SVec $ zipWith (B..|.) a b
orv (EVec a) (EVec b) = EVec $ List.nub . List.sort $ a ++ b
orv u@(EVec _) v@(SVec _) = orv (toDense u) v
orv u@(SVec _) v@(EVec _) = orv u (toDense v)


instance Vector Vec where
  add = orv

-- | Xor to find difference between vectors

xorv :: Vec -> Vec -> Vec
xorv (SVec a) (SVec b) = SVec $ zipWith B.xor a b
xorv (EVec a) (EVec b) = let us = Set.fromDistinctAscList $ List.nub $ List.sort a 
                             vs = Set.fromDistinctAscList $ List.nub $ List.sort b
                         in EVec $ Set.toList $ Set.difference us vs
xorv u@(SVec _) v@(EVec _) = xorv u (toDense v)
xorv u@(EVec _) v@(SVec _) = xorv (toDense u) v

instance Metric Vec Int where
  distance = popop xorv


-- | And to multiply

andv :: Vec -> Vec -> Vec
andv (SVec a) (SVec b) = SVec $ zipWith (B..&.) a b
andv (EVec u) (EVec v) = let us = Set.fromDistinctAscList $ List.nub $ List.sort u 
                             vs = Set.fromDistinctAscList $ List.nub $ List.sort v
                         in EVec $ Set.toList $ Set.intersection us vs
andv u@(EVec _) v@(SVec _) = xorv (toDense u) v
andv u@(SVec _) v@(EVec _) = xorv u (toDense v)


instance InnerProduct Vec Int where
  inner = popop mul -- TODO: dotv...
  mul = andv


{-
wrotv :: SVec -> Int -> SVec
dot :: SVec -> SVec  
wedge ::
-}


