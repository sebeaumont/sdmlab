{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

-- Copyright (C) Simon Beaumont - All Rights Reserved

{-|
--------------------------------------------------------------------
Module      : Database.SDM
Description : SDM Database module
Copyright   : Simon Beaumont
License     : Research Use only - *All Rights Reserved*
Stability   : Experimental
Portability : unknown
--------------------------------------------------------------------

SDM database functionality is exposed through several mircoservices
attached to various zmq endpoints with message semantics appropriate
to the function.

-}

module Database.SDM (Query, neighbours, example1) where 

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.MessagePack (MessagePack, Object, toObject, fromObject, pack, unpack)

-- ok bottom up lets create a record type thats like something we need
-- to get neighbours
-- data Query = Match | FK  

{--

type Space = Text
data Point = Point { name :: Text }
data Subspace = Subspace { space :: Space, points :: [Point] }

type Measure = Subspace -> Double
type Metric = Point -> Point -> Double

--}
  
data Query = Measure { srcSpace :: !Text
                     , srcName :: !Text
                     -- space to measure
                     , tgtSpace :: !Text
                     -- metric over tgtSpace
                     , similarityLB :: !Double
                     -- filter
                     , densityUB :: !Double
                     -- constrain
                     , maxCard :: !Int
                     }
             
           -- watch this space
           | Foobar { f1 :: !Text}
           
  deriving (Show, Read)


-- result data

data Result =  PointSet { points :: [Point] }

data Point = Point { name :: !Text
                   , similarity :: !Double
                   , density :: !Double
                   } deriving (Show, Read)





data Failed = Fail { message :: Text }


-- | a function to compute the results of a query 

neighbours :: Query -> Either Failed Result
neighbours Measure{..} = Right (PointSet [Point "foo" 0.5 0.4])
  --[srcSpace, srcName, tgtSpace, similarityLB, densityUB, maxCard)
-- neighbours _ = Left (Fail "Not implemented")


-- to implement the MessagePack protocol for the out bound messsage

instance MessagePack Query where

  toObject :: Query -> Object
  -- turns out we can wrap tuples
  toObject Measure{..} = toObject (srcSpace, srcName, tgtSpace, similarityLB, densityUB, maxCard)
  
  fromObject :: Object -> Maybe Query
  fromObject _ = Nothing  -- todo Just Query {}


instance MessagePack Result where
  -- dont' need this as is a response
  toObject :: Result -> Object
  --toObject Result{..} = toObject points
  toObject = undefined
  
  fromObject :: Object -> Maybe Result
  fromObject = undefined
  
-- {-# INLINE #-}

totuple :: Query -> (Text, Text, Text, Double, Double, Int)
totuple Measure{..} = (srcSpace, srcName, tgtSpace, similarityLB, densityUB, maxCard)


example1 :: Query
example1 = Measure { srcSpace = "words"
                   , srcName = "Football"
                   , tgtSpace = "words"
                   , similarityLB = 0.7
                   , densityUB = 0.4
                   , maxCard = 20
                   }

