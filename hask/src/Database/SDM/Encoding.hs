{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

-- Copyright (C) 2016 Simon Beaumont - All Rights Reserved

{-|
--------------------------------------------------------------------
Module      : Database.SDM.Encoding
Description : SDM message encoding module
Copyright   : Simon Beaumont
License     : Research Use only - *All Rights Reserved*
Stability   : Experimental
Portability : unknown
--------------------------------------------------------------------

SDM database functionality is exposed through several mircoservices
attached to various zmq endpoints with message semantics appropriate
to the function.

-}

module Database.SDM.Encoding (toJSON, fromJSON, toMsgPck, fromMsgPck) where

import Database.SDM.Messages
import qualified Data.ByteString.Lazy as B
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import Data.MessagePack (MessagePack, pack, unpack)
  

-- to implement the MessagePack encoding for the RPC messsages

instance MessagePack Request
instance MessagePack Point
instance MessagePack Reply


-- to implement the JSON encoding for the RPC messages

instance FromJSON Request
instance FromJSON Point
instance FromJSON Reply

instance ToJSON Request
instance ToJSON Point
instance ToJSON Reply


-- | test that the encoding/decoding is invertable

symReqJSON :: Request -> Maybe Request
symReqJSON = decode . encode

symReqMsgPck :: Request -> Maybe Request
symReqMsgPck = unpack . pack

symRepJSON :: Reply -> Maybe Reply
symRepJSON = decode . encode

symRepMsgPck :: Reply -> Maybe Reply
symRepMsgPck = unpack . pack

-- | the encoding functions

toJSON :: Request -> B.ByteString  
toJSON = encode

fromJSON :: B.ByteString -> Maybe Reply
fromJSON = decode


toMsgPck :: Request -> B.ByteString
toMsgPck = pack

fromMsgPck :: B.ByteString -> Maybe Reply
fromMsgPck = unpack
