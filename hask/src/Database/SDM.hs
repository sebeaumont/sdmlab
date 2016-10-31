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

module Database.SDM (Subspace, example1) where 

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.MessagePack (MessagePack, Object, toObject, fromObject, pack, unpack)

-- ok bottom up lets create a record type thats like something we need
-- to get neighbours

data Subspace = Subspace { srcSpace :: Text
                         , srcName :: Text
                         , tgtSpace :: Text
                         , similarityLB :: Double
                         , densityUB :: Double
                         , maxCard :: Int
                         } deriving (Show, Read)

data Neighbour = Neighbour { name :: Text
                           , similarity :: Double
                           , density :: Double
                           } deriving (Show, Read)

data Neighbourhood =  Neighbourhood { neighbours :: [Neighbour] }

data Failed = Fail { message :: Text }


-- | 
measure :: Subspace -> Either Failed Neighbourhood
measure Subspace{..} = Right (Neighbourhood [Neighbour "foo" 0.5 0.4])
  --[srcSpace, srcName, tgtSpace, similarityLB, densityUB, maxCard)
measure _ = Left (Fail "Not implemented")

-- is this record a tuple? no bah...  so we need to invent a typeclass
-- to implement the MessagePack protocol

instance MessagePack Subspace where

  toObject :: Subspace -> Object
  -- turns out we can wrap tuples
  toObject Subspace{..} = toObject (srcSpace, srcName, tgtSpace, similarityLB, densityUB, maxCard)
  
  fromObject :: Object -> Maybe Subspace
  fromObject _ = Nothing  -- todo Just Subspace {}

  
-- {-# INLINE #-}

totuple :: Subspace -> (Text, Text, Text, Double, Double, Int)
totuple Subspace{..} = (srcSpace, srcName, tgtSpace, similarityLB, densityUB, maxCard)


example1 :: Subspace
example1 = Subspace { srcSpace = "words"
                    , srcName = "Football"
                    , tgtSpace = "words"
                    , similarityLB = 0.7
                    , densityUB = 0.4
                    , maxCard = 20
                    }

