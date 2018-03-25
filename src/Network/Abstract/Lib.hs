module Network.Abstract.Lib ( createUDPNode
                            , createTCPNode
                            ) where


import           Control.Concurrent.Chan.Unagi
import           Control.Monad.State.Strict
import qualified Data.ByteString                as B
import qualified Data.HashMap.Strict            as H
import           Network.Abstract.TCPNetContext
import           Network.Abstract.Types
import           Network.Abstract.UDPNetContext

createUnc :: NetContext t => t -> NetAddr -> UserNetContext
createUnc ctx addr =
  UserNetContext { sendMsg  = sendMsgInternal ctx addr
                 , getReply = getReplyInternal ctx addr
                 , selfAddr = addr
                 }

createUDPNode :: MonadIO m => NetAddr -> UserNetHandler () -> m UserNetContext
createUDPNode addr handler = do
  let uncs = createUnc udpn addr
      udpn = UDPNetContext addr handler
  liftIO $ udpNetContextListen udpn
  return uncs

createTCPNode :: MonadIO m => NetAddr -> UserNetHandler B.ByteString -> m UserNetContext
createTCPNode addr handler = do
  let uncs = createUnc tcpn addr
      tcpn = TCPNetContext addr handler
  liftIO $ tcpNetContextListen tcpn
  return uncs
