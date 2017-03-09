
module Database.SDM.Query where

import Database.SDM.Internal.Decode -- serialization API 
import Database.SDM.Internal.SDMLIB -- FFI
import Database.SDM.Algebra (SVec)  -- semantic vector type 

-- | Basic database queries

-- TODO: turn maybe into either with error codes

getTerms :: SDMSpace -> String -> Int -> IO (Maybe TermMatchReply)
getTerms s p n = (bsdecode . fst) <$> sdm_space_serialize_symbols s p n


-- Attempt to create convenient API...


-- | Get semantic vector for a term.
-- e.g. fmap popv `fmap` (getSemanticVector (fst sb) "Sherlock")
--
getSemanticVector :: SDMSpace -> String -> IO (Either SDMStatus SDMBitVector)
getSemanticVector s t = do
  gv <- sdm_space_get_vector s t
  if is_error (snd gv)
    then return $ Left (snd gv)
      -- this should not fail - but we could check!
    else sdm_vector_load (fst gv) >>= return . Right . fst


-- | Get topology for a SVec a.k.a. SDMBitVector
getTopology :: SDMSpace
            -> Double
            -> Double
            -> Int
            -> SDMBitVector
            -> IO [SDMPoint]
getTopology sp s d n v = fst <$> sdm_space_get_topology sp s d n v


-- | Get basis vector
getElementalVector :: SDMSpace -> String -> IO (Either SDMStatus [SDMVectorIdx])
getElementalVector s t = do
  ss <- sdm_space_get_symbol s t 
  if is_error (snd ss)
    then return $ Left (snd ss)
    -- this should not fail but we could check!
    else return $ Right $ sdm_symbol_get_basis (fst ss) 
           


