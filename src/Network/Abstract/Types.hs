{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Network.Abstract.Types ( NetAddr(..)
                              , NetMsg(..)
                              , NetContext(..)
                              , UserNetContext(..)
                              , simpleAddrToNetAddr
                              , hostAddressToTuple
                              , tupleToHostAddress
                              ) where


import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.MVar
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Except
import           Crypto.Hash
import qualified Crypto.Number.Basic           as CNB
import qualified Crypto.Number.Serialize       as CNS
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as C
import qualified Data.Hashable                 as HASH
import qualified Data.HashMap.Strict           as H
import           Data.IP
import           Data.Serialize                as S
import           Data.Word
import           GHC.Generics                  (Generic)
import           Safe

import           Network.Socket

customHash :: Show a => a -> B.ByteString
customHash x = C.pack $ show ((hash $ C.pack $ show x) :: Digest SHA3_512)

hashToInt :: B.ByteString -> Integer
hashToInt b =
  let wrds :: [Word8] = B.unpack b
      ints :: [Integer] = fromIntegral `map` wrds
  in foldl bitproduct 0 ints
  where bitproduct acc i = acc * 256 + i

type NetAddr = SockAddr
type NetMsg  = (NetAddr, B.ByteString)

deriving instance Read NetAddr
deriving instance Generic NetAddr

instance Serialize PortNumber where
  put = putPort
  get = getPort

putPort :: Putter PortNumber
putPort port =
  let port_ = fromIntegral port :: Int
  in S.put port_

getPort :: Get PortNumber
getPort = S.get

instance Serialize NetAddr where
  put = putNetAddr
  get = getNetAddr

putNetAddr :: Putter NetAddr
putNetAddr (SockAddrInet port host) = do
  S.put port
  S.put host

getNetAddr :: Get NetAddr
getNetAddr = do
  port <- S.get
  host <- S.get
  return $ SockAddrInet port host

simpleAddrToNetAddr :: (String, Int) -> Either String NetAddr
simpleAddrToNetAddr (shost, iport) =
  let port_ = fromIntegral iport :: PortNumber
  in case (readMay shost :: Maybe IPv4) of
    Nothing -> Left "Could not parse string address"
    Just ipHost ->
      let host_ = toHostAddress ipHost
      in Right $ SockAddrInet port_ host_

instance HASH.Hashable NetAddr where
  hashWithSalt salt addr =
    fromInteger . hashToInt . customHash $ show salt ++ show addr

class NetContext t where
  sendMsgInternal :: MonadIO m => t -> NetAddr -> NetAddr -> B.ByteString -> m ()
  getReplyInternal :: MonadIO m => t -> NetAddr -> NetAddr -> B.ByteString -> m B.ByteString

data UserNetContext =
  UserNetContext { sendMsg  :: NetAddr -> B.ByteString -> IO ()
                 , getReply :: NetAddr -> B.ByteString -> IO B.ByteString
                 , selfAddr :: NetAddr
                 }

type Handler = (NetAddr, B.ByteString) -> IO B.ByteString
