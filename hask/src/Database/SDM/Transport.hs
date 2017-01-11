{-# LANGUAGE OverloadedStrings #-}

-- | ZMQ client side messaging based transport IO module 


module Database.SDM.Transport ( rpcConnect
                              , RPCPort
                              , rpc
                              , rpcClose
                              ) where

import qualified System.ZMQ4 as Z
import qualified Data.ByteString.Lazy as B
--import Data.Text.Encoding (encodeUtf8)
--import Data.List.NonEmpty (fromList)


--------------------------------------------
-- | encapsulates a query-response endpoint
--------------------------------------------

data RPCPort = CP { _context :: Z.Context
                  , _cmdSocket :: Z.Socket Z.Req
                  , endpoint :: String
                  } 

instance Show RPCPort where 
  show (CP _ _ e) = show e


-- | RPC connection factory
    
rpcConnect :: String               -- ^ Transport connection string. e.g. tcp://localhost
        -> IO RPCPort
rpcConnect endpoint = do
  ctx <- Z.context 
  sok <- Z.socket ctx Z.Req
  Z.connect sok endpoint
  return $ CP ctx sok endpoint


-- | Close 'RPCPort'
  
rpcClose :: RPCPort -> IO ()
rpcClose cp = Z.close (_cmdSocket cp) >> Z.shutdown (_context cp) >> Z.term (_context cp)

-- | Synchronous rpc

rpc :: RPCPort -> B.ByteString -> IO B.ByteString
rpc port message = Z.send' (_cmdSocket port) [] message
  >> Z.receive (_cmdSocket port) >>= return . B.fromStrict
{-# INLINE rpc #-}


  
