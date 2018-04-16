{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Network.Abstract.TCPNetContext where

import qualified Control.Concurrent.Thread  as Thread
import           Control.Exception
import           Control.Monad.State.Strict
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as C
import           Data.Int                   (Int32)
import           Network.Abstract.Types
import           Network.Socket
import qualified Network.Socket.ByteString  as NSB
import           Text.Read

runThread :: IO () -> IO ()
runThread action = do
  (_, _) <- Thread.forkIO action
  return ()

data TCPNetContext = TCPNetContext NetAddr (UserNetHandler B.ByteString)

instance NetContext TCPNetContext where
  sendMsgInternal = tcpNetContextSend
  getReplyInternal = tcpNetContextReply

createConnectedTCPSocket :: MonadIO m => NetAddr -> m Socket
createConnectedTCPSocket to = liftIO $ do
  sock <- socket AF_INET Stream defaultProtocol
  connect sock to
  return sock

sendMsgWithLen :: MonadIO m => Socket -> NetAddr -> B.ByteString -> m ()
sendMsgWithLen sock to msg = do
  let mlen :: Int32 = fromIntegral $ B.length msg
      lenBytes = intToBytes $ fromIntegral mlen
  liftIO $ do
    result :: Either IOException () <- try (NSB.sendAll sock $ C.concat [lenBytes, msg])
    case result of
      Left excp -> putStrLn $ "Error sending message to " ++ show to ++ ": " ++ displayException excp
      Right _  -> return ()

recvMsgWithLen :: MonadIO m => Socket -> m B.ByteString
recvMsgWithLen conn = liftIO $ do
  rawmsglen <- NSB.recv conn 4
  let msglen = bytesToInt rawmsglen
  NSB.recv conn msglen

tcpNetContextSend :: MonadIO m => TCPNetContext -> NetAddr -> NetAddr -> B.ByteString -> m ()
tcpNetContextSend _ from to msg = liftIO $ do
  sock <- createConnectedTCPSocket to
  sendMsgWithLen sock to $ C.pack . show $ MsgWithAddr { basemsg = msg, senderaddr = show from }
  close sock

tcpNetContextReply :: MonadIO m => TCPNetContext -> NetAddr -> NetAddr -> B.ByteString -> m B.ByteString
tcpNetContextReply _ from to msg = liftIO $ do
  sock <- createConnectedTCPSocket to
  sendMsgWithLen sock to $ C.pack . show $ MsgWithAddr { basemsg = msg, senderaddr = show from }
  finalMsg <- recvMsgWithLen sock
  close sock
  return finalMsg

server :: TCPNetContext -> IO ()
server (TCPNetContext (SockAddrInet port _) handler) = do
  addrinfos <- getAddrInfo
               (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
               Nothing (Just . (show :: Int -> String) . fromIntegral $ port)
  let serveraddr = head addrinfos

  -- Create a socket
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol

  -- Bind it to the address we're listening to
  bind sock (addrAddress serveraddr)

  -- 50 max connections
  listen sock 50

  -- Loop forever processing incoming data.  Ctrl-C to abort.
  result :: Either IOException () <- try (procMessages sock)
  case result of
    Left err -> putStrLn $ "Error occured in TCP server: " ++ displayException err
    Right _  -> return ()
  close sock
    where procMessages sock = do
            (conn, addr) <- accept sock
            rawmsg <- recvMsgWithLen conn

            let result = do
                  parsedMsg <- readEither . C.unpack $ rawmsg
                  address <- stringToNetAddr $ senderaddr parsedMsg
                  return (address, basemsg parsedMsg)

            case result of
              Left err  -> putStrLn $ "Error: " ++ err
              Right res -> do
                resp <- handler res
                unless (B.null resp) $ sendMsgWithLen conn addr resp
                close conn

            procMessages sock
server _ = error "Fatal error. Invalid TCPNetContext received"

tcpNetContextListen :: TCPNetContext -> IO ()
tcpNetContextListen ctx = do
  runThread $ do
    result :: Either IOException () <- try (server ctx)
    case result of
      Left err -> putStrLn $ "Could not start server: " ++ displayException err
      Right _  -> return ()
  return ()
