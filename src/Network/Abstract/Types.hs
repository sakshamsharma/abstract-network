{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as C
import qualified Data.Hashable                 as HASH
import qualified Data.HashMap.Strict           as H
import           Data.IP
import           Data.Word
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

simpleAddrToNetAddr :: (String, Int) -> Either String NetAddr
simpleAddrToNetAddr (shost, iport) = do
  let port_ = fromIntegral iport :: PortNumber
  case (readMay shost :: Maybe IPv4) of
    Nothing -> Left "Could not parse string address"
    Just ipHost ->
      let host_ = toHostAddress ipHost
      in Right $ SockAddrInet port_ host_

instance HASH.Hashable NetAddr where
  hashWithSalt salt addr =
    fromInteger . hashToInt . customHash $ show salt ++ show addr

class NetContext t where
  sendMsgInternal :: MonadIO m => t -> NetAddr -> NetAddr -> B.ByteString -> m ()

data UserNetContext m =
  UserNetContext { sendMsg  :: MonadIO m => NetAddr -> B.ByteString -> m ()
                 , msgQueue :: OutChan NetMsg
                 , selfAddr :: NetAddr
                 }
