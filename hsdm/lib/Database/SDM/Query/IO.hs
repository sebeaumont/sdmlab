
module Database.SDM.Query.IO
  (openDB,
   SDMStatus,
   SDMDatabase,
   closeDB,
   ensureSpace,
   getSpace,
   SDMSpace,
   -- get vectors
   getSemanticVector,
   getElementalVector,
   -- level sets
   getTopology,
   --getTopologyForTerm,
   SDMPoint,
   SDMCard,
   -- terms - consider refactor
   getTerms,
   TermMatchReply,
   TermMatch,
   Term
  ) where

import Database.SDM.Internal.Decode -- serialization API 
import Database.SDM.Internal.SDMLIB -- FFI
import Database.SDM.Algebra         

-- | Basic database queries and utilities


openDB :: String -> Int -> Int -> IO (Either SDMStatus SDMDatabase)
openDB f sz ms = do
  dv <- sdm_database f sz ms
  if is_error (snd dv)
    then return $ Left (snd dv)
    else return $ Right (fst dv)


closeDB :: SDMDatabase -> IO ()
closeDB = sdm_database_close


ensureSpace :: SDMDatabase -> String -> IO (Either SDMStatus SDMSpace)
ensureSpace db s = do
  sv <- sdm_database_ensure_space db s
  if is_error (snd sv)
    then return $ Left (snd sv)
    else return $ Right (fst sv)

getSpace :: SDMDatabase -> String -> IO (Either SDMStatus SDMSpace)
getSpace db s = do
  sv <- sdm_database_get_space db s
  if is_error (snd sv)
    then return $ Left (snd sv)
    else return $ Right (fst sv)
    

-- TODO: turn maybe into either with error codes... see bsdecode.

getTerms :: SDMSpace -> String -> Int -> IO (Maybe TermMatchReply)
getTerms s p n = (bsdecode . fst) <$> sdm_space_serialize_symbols s p n


-- Attempt to create a convenient API...


-- | Get semantic vector for a term.
getSemanticVector :: SDMSpace -> String -> IO (Either SDMStatus Vec)
getSemanticVector s t = do
  gv <- sdm_space_get_vector s t
  if is_error (snd gv)
    then return $ Left (snd gv)
    -- this should not fail - but we could check!
    else sdm_vector_load (fst gv) >>= return . Right . SVec . fst



-- | Get topology for a dense pattern vector

getTopology :: SDMSpace
            -> Double
            -> Double
            -> Int
            -> Vec
            -> IO ([SDMPoint], SDMCard)
getTopology sp s d n (SVec v) = sdm_space_get_topology sp s d n v
getTopology sp s d n v@(EVec _) = getTopology sp s d n (toDense v)


-- | Get basis vector
getElementalVector :: SDMSpace -> String
                   -> IO (Either SDMStatus Vec)
getElementalVector s t = do
  ss <- sdm_space_get_symbol s t 
  if is_error (snd ss)
    then return $ Left (snd ss)
    -- this should not fail but we could check!
    else return $ Right $ EVec $ sdm_symbol_get_basis (fst ss) 


  
{-
getTopologyForTerm :: SDMSpace -> Double -> Double -> Int -> String
                   -> IO (Either SDMStatus [SDMPoint])
getTopologyForTerm s m d n t = do
  semv <- getSemanticVector s t
  case semv of
    Right v -> getTopology s m d n v >>= return . Right 
    Left e -> return $ Left e
-}
