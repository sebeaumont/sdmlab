module Database.SDM.Bootstrap where

import Database.SDM.Internal.SDMLIB

-- showtime...


initDB :: IO (Either SDMStatus SDMDatabase)
initDB = do
  (db, sts) <- sdm_database "/Users/seb/Data/ash.sdm" (1024*1024*700) (1024*1024*700)
  if (sts < 0)
    then return $ Left sts
    else return $ Right db

{-
 + could wrap io to encapsulate all the side effects of calling SDM IO functions
-}
