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
  getReplyInternal = undefined

tcpNetContextSend :: MonadIO m => TCPNetContext -> NetAddr -> NetAddr -> B.ByteString -> m ()
tcpNetContextSend ctx from to msg = liftIO $ do
  sock <- socket AF_INET Stream defaultProtocol
  connect sock to
  NSB.sendAll sock msg
  close sock

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
    where procMessages sock =
              do
                (sock, addr) <- accept sock
                msg <- NSB.recv sock 0
                handlerfunc addr msg
                procMessages sock
          handlerfunc addr msg = handler (addr, msg)

tcpNetContextListen :: TCPNetContext -> IO ()
tcpNetContextListen ctx = do
  runThread $ server ctx
  return ()
