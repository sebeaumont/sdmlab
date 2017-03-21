
module Database.SDM.Query where

import Database.SDM.Internal.Decode -- serialization API 
import Database.SDM.Internal.SDMLIB -- FFI
import Database.SDM.Algebra (Vec(SVec,EVec)) -- Vector types

-- | Basic database queries

-- TODO: turn maybe into either with error codes

getTerms :: SDMSpace -> String -> Int -> IO (Maybe TermMatchReply)
getTerms s p n = (bsdecode . fst) <$> sdm_space_serialize_symbols s p n


-- Attempt to create convenient API...

{- |
We can lift our pure vector functions up into the IO (Either SDMStatus a) type e.g:

fmap popv <$> (liftA2 orv <$> (getSemanticVector (fst sp) "Sherlock")
                          <*> (getSemanticVector (fst sp) "Simon"))

liftA2 (popop orv) <$> getSemanticVector (fst sp) "Simon" <*> getSemanticVector (fst sp) "Sherlock"

-}

-- | Get semantic vector for a term.
getSemanticVector :: SDMSpace -> String -> IO (Either SDMStatus Vec)
getSemanticVector s t = do
  gv <- sdm_space_get_vector s t
  if is_error (snd gv)
    then return $ Left (snd gv)
      -- this should not fail - but we could check!
    else sdm_vector_load (fst gv) >>= return . Right . SVec . fst



-- | Get topology for a dense pattern in SVec

getTopology :: SDMSpace
            -> Double
            -> Double
            -> Int
            -> Vec
            -> IO [SDMPoint]
getTopology sp s d n (SVec v) = fst <$> sdm_space_get_topology sp s d n v


-- | Get basis vector
getElementalVector :: SDMSpace -> String
                   -> IO (Either SDMStatus Vec)
getElementalVector s t = do
  ss <- sdm_space_get_symbol s t 
  if is_error (snd ss)
    then return $ Left (snd ss)
    -- this should not fail but we could check!
    else return $ Right $ EVec $ sdm_symbol_get_basis (fst ss) 


-- | step 1. basic term search
--
getTopologyForTerm :: SDMSpace -> Double -> Double -> Int -> String
                   -> IO (Either SDMStatus [SDMPoint])
getTopologyForTerm s m d n t = do
  semv <- getSemanticVector s t
  case semv of
    Right v -> getTopology s m d n v >>= return . Right 
    Left e -> return $ Left e

