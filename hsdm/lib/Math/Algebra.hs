{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{- |
 Vector Space Algebra
 Some typeclasses which generalise operations over vector spaces.
-}

module Math.Algebra
  (Vector,
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
   distance
  ) where

{-
 These typeclasses should really paramtertise two types or more: the
 vector and scalar types for the algebra, we may also need a type
 family of k-vectors or k-blades before embarking on that journey we
 need to take account of other work in this area, e.g. hLearn
 etc. maybe if these provide usuable typeclasses that enable us to
 leverage vector space based ML algorithms provided there these
 typeclasses can be elided.
-}

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

