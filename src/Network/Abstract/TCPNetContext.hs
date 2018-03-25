{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Network.Abstract.TCPNetContext where

import           Control.Concurrent.MVar
import qualified Control.Concurrent.Thread  as Thread
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Except
import           Data.Bits
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as C
import qualified Data.HashMap.Strict        as H
import           Data.Int                   (Int32)
import           Data.List
import           Data.Serialize
import           Network.Abstract.Types
import           Network.Socket
import qualified Network.Socket.ByteString  as NSB
import           Text.Read

runThread action = do
  (_, w) <- Thread.forkIO action
  return w

data TCPNetContext = TCPNetContext NetAddr (UserNetHandler B.ByteString)

instance NetContext TCPNetContext where
  sendMsgInternal = tcpNetContextSend
  getReplyInternal = tcpNetContextReply

createConnectedTCPSocket :: MonadIO m => NetAddr -> m Socket
createConnectedTCPSocket to = liftIO $ do
  sock <- socket AF_INET Stream defaultProtocol
  connect sock to
  return sock

sendMsgWithLen :: MonadIO m => Socket -> B.ByteString -> m ()
sendMsgWithLen sock msg = do
  let mlen :: Int32 = fromIntegral $ B.length msg
      lenBytes = intToBytes $ fromIntegral mlen
  liftIO $ NSB.sendAll sock $ C.concat [lenBytes, msg]

recvMsgWithLen :: MonadIO m => Socket -> m B.ByteString
recvMsgWithLen conn = liftIO $ do
  rawmsglen <- NSB.recv conn 4
  let msglen = bytesToInt rawmsglen
  NSB.recv conn msglen

tcpNetContextSend :: MonadIO m => TCPNetContext -> NetAddr -> NetAddr -> B.ByteString -> m ()
tcpNetContextSend ctx from to msg = liftIO $ do
  sock <- createConnectedTCPSocket to
  sendMsgWithLen sock $ C.pack . show $ MsgWithAddr { basemsg = msg, senderaddr = show from }
  close sock

tcpNetContextReply :: MonadIO m => TCPNetContext -> NetAddr -> NetAddr -> B.ByteString -> m B.ByteString
tcpNetContextReply ctx from to msg = liftIO $ do
  sock <- createConnectedTCPSocket to
  sendMsgWithLen sock $ C.pack . show $ MsgWithAddr { basemsg = msg, senderaddr = show from }
  msg <- recvMsgWithLen sock
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
            rawmsg <- recvMsgWithLen conn

            let result = do
                  parsedMsg <- readEither . C.unpack $ rawmsg
                  addr <- stringToNetAddr $ senderaddr parsedMsg
                  return (addr, basemsg parsedMsg)

            case result of
              Left err  -> putStrLn $ "Error: " ++ err
              Right res -> do
                resp <- handler res
                unless (B.null resp) $ sendMsgWithLen conn resp
                close sock

            procMessages sock

tcpNetContextListen :: TCPNetContext -> IO ()
tcpNetContextListen ctx = do
  runThread $ server ctx
  return ()
