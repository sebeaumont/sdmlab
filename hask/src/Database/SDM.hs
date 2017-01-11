{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE DeriveAnyClass #-}

-- Copyright (C) 2016 Simon Beaumont - All Rights Reserved

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

module Database.SDM (neighbours, example1) where 

import Database.SDM.Encoding
import Database.SDM.Transport
import Database.SDM.Messages

-- | define functions in the api

-- n.b. Request is too generic for strong typing of the i/f

neighbours :: RPCPort -> Request -> IO (Maybe Reply)
neighbours p r = rpc p (toMsgPck r) >>= (return . fromMsgPck)


-- | a sample request

example1 :: Request
example1 = Neighbours { srcSpace = "words"
                      , srcName = "Football"
                      , tgtSpace = "words"
                      , similarityLB = 0.7
                      , densityUB = 0.4
                      , maxCard = 20
                      }



-- | test function to compute the results of a query 

neighbours1 :: Request -> Reply
neighbours1 Neighbours{..} = PointSet tgtSpace [Point "foo" 0.5 0.4]
  --[srcSpace, srcName, tgtSpace, similarityLB, densityUB, maxCard)
neighbours1 _ = Fail "Not implemented"

