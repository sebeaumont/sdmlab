
module Database.SDM.Query where

import Database.SDM.Internal.Decode
import Database.SDM.Internal.SDMLIB

-- | Basic database queries

getTerms :: SDMSpace -> String -> Int -> IO (Maybe TermMatchReply)
getTerms s p n = (bsdecode . fst) <$> sdm_space_get_symbols s p n --  :: Maybe TermMatchReply

