{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FakeNetContext where


import           Control.Monad.State.Strict
import           Control.Monad.Trans.Except

import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.MVar

import qualified Data.ByteString               as B
import qualified Data.HashMap.Strict           as H

import           Types

data FakeNetContext =
  FakeNetContext { nodes :: H.HashMap NetAddr (InChan NetMsg) }

instance NetContext FakeNetContext where
  sendMsgInternal = fakeNetContextSend

fakeNetContextSend :: MonadIO m => FakeNetContext -> NetAddr -> NetAddr -> B.ByteString -> m ()
fakeNetContextSend ctx from to msg =
  let sendToChan c = writeChan c (from, msg)
      chans        = H.lookup to (nodes ctx)
  in case chans of
    Nothing  -> error $ "You should not be sending messages to unknown targets."
    Just inC -> liftIO $ sendToChan inC
