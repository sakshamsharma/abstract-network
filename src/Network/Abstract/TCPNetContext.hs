{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Abstract.TCPNetContext where

import           Control.Concurrent.MVar
import qualified Control.Concurrent.Thread  as Thread
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Except
import           Data.Bits
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as C
import qualified Data.HashMap.Strict        as H
import           Data.List
import           Network.Abstract.Types
import           Network.Socket
import qualified Network.Socket.ByteString  as NSB

runThread action = do
  (_, w) <- Thread.forkIO action
  return w

data TCPNetContext = TCPNetContext NetAddr ((NetAddr, B.ByteString) -> IO B.ByteString)

instance NetContext TCPNetContext where
  sendMsgInternal = tcpNetContextSend
  getReplyInternal = tcpNetContextReply

createConnectedTCPSocket :: MonadIO m => NetAddr -> m Socket
createConnectedTCPSocket to = liftIO $ do
  sock <- socket AF_INET Stream defaultProtocol
  connect sock to
  return sock

tcpNetContextSend :: MonadIO m => TCPNetContext -> NetAddr -> NetAddr -> B.ByteString -> m ()
tcpNetContextSend ctx from to msg = liftIO $ do
  sock <- createConnectedTCPSocket to
  NSB.sendAll sock msg
  close sock

tcpNetContextReply :: MonadIO m => TCPNetContext -> NetAddr -> NetAddr -> B.ByteString -> m B.ByteString
tcpNetContextReply ctx from to msg = liftIO $ do
  sock <- createConnectedTCPSocket to
  NSB.sendAll sock msg
  msg <- NSB.recv sock 0
  close sock
  return msg

server :: TCPNetContext -> IO ()
server (TCPNetContext (SockAddrInet port _) handler) = do
  addrinfos <- getAddrInfo
               (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
               Nothing (Just . show . fromIntegral $ port)
  let serveraddr = head addrinfos

  -- Create a socket
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol

  -- Bind it to the address we're listening to
  bind sock (addrAddress serveraddr)

  -- 50 max connections
  listen sock 50

  -- Loop forever processing incoming data.  Ctrl-C to abort.
  procMessages sock
  close sock
    where procMessages sock = do
            (conn, addr) <- accept sock
            msg <- NSB.recv conn 0
            reply <- handler (addr, msg)
            unless (B.null reply) $ NSB.sendAll conn reply
            close conn
            procMessages sock

tcpNetContextListen :: TCPNetContext -> IO ()
tcpNetContextListen ctx = do
  runThread $ server ctx
  return ()
