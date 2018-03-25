{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Network.Abstract.UDPNetContext where


import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.MVar
import qualified Control.Concurrent.Thread     as Thread
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Except
import           Data.Bits
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as C
import qualified Data.HashMap.Strict           as H
import           Data.List
import           Data.Serialize
import           Data.Word
import           Network.Abstract.Types
import           Network.Socket
import qualified Network.Socket.ByteString     as NSB
import           Text.Read

runThread action = do
  (_, w) <- Thread.forkIO action
  return w

data UDPNetContext = UDPNetContext NetAddr (UserNetHandler ())

instance NetContext UDPNetContext where
  sendMsgInternal = udpNetContextSend
  getReplyInternal = error "getReply is not yet supported on UDP nodes"

udpNetContextSend :: MonadIO m => UDPNetContext -> NetAddr -> NetAddr -> B.ByteString -> m ()
udpNetContextSend ctx from to msg = liftIO $ do
  sock <- socket AF_INET Datagram defaultProtocol
  connect sock to
  NSB.sendAll sock $ C.pack . show $ MsgWithAddr { basemsg = msg, senderaddr = show from }
  close sock

server :: UDPNetContext -> IO ()
server (UDPNetContext (SockAddrInet port _) handler) = do
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
    where procMessages sock = do
                (rawmsg, rawaddr) <- NSB.recvFrom sock 4096
                let result = do
                      parsedMsg <- readEither . C.unpack $ rawmsg
                      addr <- stringToNetAddr $ senderaddr parsedMsg
                      return (addr, basemsg parsedMsg)

                case result of
                  Left err  -> putStrLn $ "Error: " ++ err
                  Right res -> handler res

                -- And process more messages
                procMessages sock

udpNetContextListen :: UDPNetContext -> IO ()
udpNetContextListen ctx = do
  runThread $ server ctx
  return ()
