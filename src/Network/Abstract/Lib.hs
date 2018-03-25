module Network.Abstract.Lib ( createUDPNode
                            ) where


import           Control.Concurrent.Chan.Unagi
import           Control.Monad.State.Strict
import qualified Data.HashMap.Strict            as H

-- import           Network.Abstract.FakeNetContext
import           Network.Abstract.Types
import           Network.Abstract.UDPNetContext

createUnc :: NetContext t => t -> NetAddr -> UserNetContext
createUnc ctx addr =
  UserNetContext { sendMsg  = sendMsgInternal ctx addr
                 , getReply = getReplyInternal ctx addr
                 , selfAddr = addr
                 }

createUDPNode :: MonadIO m => NetAddr -> Handler -> m UserNetContext
createUDPNode addr handler = do
  let uncs = createUnc udpn addr
      udpn = UDPNetContext addr handler
  liftIO $ udpNetContextListen udpn
  return uncs
