{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}

module Network.Abstract.Types where


import           Control.Monad.State.Strict
import           Control.Monad.Trans.Except

import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.MVar

import qualified Data.ByteString               as B
import qualified Data.HashMap.Strict           as H

type NetAddr = (String, Int)
type NetMsg  = (NetAddr, B.ByteString)

class NetContext t where
  sendMsgInternal :: MonadIO m => t -> NetAddr -> NetAddr -> B.ByteString -> m ()

data UserNetContext m =
  UserNetContext { sendMsg  :: MonadIO m => NetAddr -> B.ByteString -> m ()
                 , msgQueue :: OutChan NetMsg
                 , selfAddr :: NetAddr
                 }
