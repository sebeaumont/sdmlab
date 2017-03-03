{-# LANGUAGE DeriveGeneric #-}

{-|
Decode API JSON values
-}

module Database.SDM.Internal.Decode where

import Data.Aeson (FromJSON, decode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Text
import GHC.Generics


-- | Term and TermMatch


data Term = Term { name :: Text
                 , density :: Double
                 } deriving (Generic, Show)

data TermMatch = TermMatch { prefix :: Text
                           , matches :: Int
                           , terms :: [Term]
                           } deriving (Generic, Show)

data TermMatchReply = TermMatchReply { term_match :: TermMatch } deriving (Generic, Show)

instance FromJSON Term
instance FromJSON TermMatch
instance FromJSON TermMatchReply

-- our data is usually strict byte strings
bsdecode ::  FromJSON a => ByteString -> Maybe a
bsdecode = decode . fromStrict

