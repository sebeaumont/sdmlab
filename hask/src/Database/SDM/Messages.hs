{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

-- Copyright (C) 2016 Simon Beaumont - All Rights Reserved

{-|
--------------------------------------------------------------------
Module      : Database.SDM.Messages
Description : SDM message definition
Copyright   : Simon Beaumont
License     : Research Use only - *All Rights Reserved*
Stability   : Experimental
Portability : unknown
--------------------------------------------------------------------

SDM database functionality is exposed through several mircoservices
attached to various zmq endpoints with message semantics appropriate
to the function.

-}

module Database.SDM.Messages (Request(..), Reply(..), Point(..)) where


import GHC.Generics (Generic)
import Data.ByteString (ByteString)
import Data.Text (Text)


{-
This feels too generic as we really need distinct types fpr each message
-}
       
data Request = Neighbours { srcSpace :: !Text
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
           
             deriving (Show, Generic)


-- result data

data Reply = PointSet { space :: !Text, points :: [Point] }
           | Fail { message :: !Text }
            deriving (Show, Generic)


data Point = Point { name :: !Text
                   , similarity :: !Double
                   , density :: !Double
                   } deriving (Show, Generic)
