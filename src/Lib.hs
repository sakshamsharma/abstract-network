module Lib where


import           Control.Concurrent.Chan.Unagi

import qualified Data.HashMap.Strict           as H

import           FakeNetContext
import           Types

createUnc :: NetContext t => t -> (NetAddr, OutChan NetMsg) -> UserNetContext
createUnc ctx (addr, outChan) =
  UserNetContext { sendMsg  = (sendMsgInternal ctx) addr
                 , msgQueue = outChan
                 , selfAddr = addr
                 }

createFakeNetwork :: [NetAddr] -> IO [UserNetContext]
createFakeNetwork addrs = do
  qs <- mapM (\_ -> newChan) addrs
  let inChans  = fst `map` qs
      outChans = snd `map` qs
      nodes_   = zip addrs inChans
      uncs_    = zip addrs outChans
      fn       = FakeNetContext { nodes = H.fromList nodes_ }
      uncs     = createUnc fn `map` uncs_
  return uncs
