module Lib where


import           Control.Monad.State.Strict

import           Control.Concurrent.Chan.Unagi

import qualified Data.HashMap.Strict           as H

import           FakeNetContext
import           Types

createUnc :: (MonadIO m, NetContext t) => t -> (NetAddr, OutChan NetMsg) -> UserNetContext m
createUnc ctx (addr, outChan) =
  UserNetContext { sendMsg  = (sendMsgInternal ctx) addr
                 , msgQueue = outChan
                 , selfAddr = addr
                 }

createFakeNetwork :: MonadIO m => [NetAddr] -> IO [UserNetContext m]
createFakeNetwork addrs = do
  qs <- mapM (\_ -> newChan) addrs
  let inChans  = fst `map` qs
      outChans = snd `map` qs
      nodes_   = zip addrs inChans
      uncs_    = zip addrs outChans
      fn       = FakeNetContext { nodes = H.fromList nodes_ }
      uncs     = createUnc fn `map` uncs_
  return uncs
