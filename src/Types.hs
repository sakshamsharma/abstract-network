{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types where


import           Control.Monad.State.Strict
import           Control.Monad.Trans.Except

import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.MVar

import qualified Data.ByteString               as B
import qualified Data.HashMap.Strict           as H

type NetAddr = (String, Int)
type NetMsg  = (NetAddr, B.ByteString)

class NetContext t where
  sendMsgInternal :: t -> NetAddr -> NetAddr -> B.ByteString -> IO ()

data UserNetContext =
  UserNetContext { sendMsg  :: NetAddr -> B.ByteString -> IO ()
                 , msgQueue :: OutChan NetMsg
                 , selfAddr :: NetAddr
                 }
