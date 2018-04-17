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
                              , UserNetHandler
                              , MsgWithAddr(..)
                              , putNetAddr
                              , getNetAddr
                              , simpleAddrToNetAddr
                              , stringToNetAddr
                              , hostAddressToTuple
                              , intToBytes
                              , bytesToInt
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
import           Data.List.Split
import           Data.Serialize                as S
import           Data.Word
import           GHC.Generics                  (Generic)
import           Network.Socket
import           Safe
import           Text.Read

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

-- TODO: Read instance is wrong, and thus, doesn't work.
-- This is because the Show instance gives a fancy printed output.
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

stringToNetAddr :: String -> Either String NetAddr
stringToNetAddr str = do
  let x = splitOn ":" str
  (h, pt) <- case x of
    [h,pt] -> Right(h, pt)
    _      -> Left "Bad format for string"
  port <- readEither pt
  simpleAddrToNetAddr (h, port)

simpleAddrToNetAddr :: (String, Int) -> Either String NetAddr
simpleAddrToNetAddr (shost, iport) =
  let port_ = fromIntegral iport :: PortNumber
  in case (readMay shost :: Maybe IPv4) of
    Nothing -> Left "Could not parse string address"
    Just ipHost ->
      let host_ = toHostAddress ipHost
      in Right $ SockAddrInet port_ host_

intToBytes :: Int -> B.ByteString
intToBytes i =
  let i1 = i `mod` 256
      i2 = ((i-i1) `div` 256) `mod` 256
      i3 = ((i-i2) `div` 256) `mod` 256
      i4 = ((i-i3) `div` 256) `mod` 256
  in B.pack [fromIntegral i4, fromIntegral i3, fromIntegral i2, fromIntegral i1]

bytesToInt :: B.ByteString -> Int
bytesToInt = helper . B.unpack
  where helper [i4, i3, i2, i1] =
          fromIntegral i1 + 256 * (fromIntegral i2 + 256 * (fromIntegral i3 + 256 * fromIntegral i4))

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

type UserNetHandler ret = (NetAddr, B.ByteString) -> IO ret

data MsgWithAddr = MsgWithAddr { basemsg    :: B.ByteString
                               , senderaddr :: String
                               } deriving (Show, Read)
