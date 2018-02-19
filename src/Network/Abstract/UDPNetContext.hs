{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Abstract.UDPNetContext where


import           Control.Monad.State.Strict
import           Control.Monad.Trans.Except

import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.MVar
import qualified Control.Concurrent.Thread     as Thread

import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as C
import qualified Data.HashMap.Strict           as H

import           Network.Abstract.Types

import           Data.Bits
import           Data.List
import           Network.Socket
import qualified Network.Socket.ByteString     as NSB

runThread action = do
  (_, w) <- Thread.forkIO action
  return w

data UDPNetContext = UDPNetContext NetAddr (InChan NetMsg)

instance NetContext UDPNetContext where
  sendMsgInternal = udpNetContextSend

udpNetContextSend :: MonadIO m => UDPNetContext -> NetAddr -> NetAddr -> B.ByteString -> m ()
udpNetContextSend ctx from to msg = liftIO $ do
  sock <- socket AF_INET Datagram defaultProtocol
  connect sock to
  NSB.sendAll sock msg
  close sock

server (UDPNetContext (SockAddrInet port _) inchan) = do
  addrinfos <- getAddrInfo
               (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
               Nothing (Just . show . fromIntegral $ port)
  let serveraddr = head addrinfos

  -- Create a socket
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

  -- Bind it to the address we're listening to
  bind sock (addrAddress serveraddr)

  -- Loop forever processing incoming data.  Ctrl-C to abort.
  procMessages sock
  close sock
    where procMessages sock =
              do -- Receive one UDP packet, maximum length 1024 bytes,
                 -- and save its content into msg and its source
                 -- IP and port into addr
                 (msg, addr) <- NSB.recvFrom sock 4096
                 -- Handle it
                 handlerfunc addr msg
                 -- And process more messages
                 procMessages sock
          handlerfunc addr msg = do
            writeChan inchan (addr, msg)

udpNetContextListen :: UDPNetContext -> IO ()
udpNetContextListen ctx = do
  runThread $ server ctx
  return ()
