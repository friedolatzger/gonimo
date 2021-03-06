{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}


module Gonimo.Server.Subscriber.Types where

import           Control.Concurrent.STM
import           Data.Map (Map)

import           Gonimo.SocketAPI

type ReferenceCount = Int
type Revision = Int
type ResourceStatusMap = Map ServerRequest (TVar (RefCounted ResourceStatus))

data ResourceStatus = Modified Revision deriving (Eq, Show)

data RefCounted a = RefCounted {
  refCount :: ReferenceCount
, refValue :: a
}

instance Functor RefCounted where
  fmap f (RefCounted c v) = RefCounted c (f v)

data Subscriber = Subscriber {
  subState   :: !(TVar ResourceStatusMap)
}


type ClientMonitors = Map ServerRequest StatusMonitor

data Client = Client {
    subscriber      :: !Subscriber
  , monitors        :: !(TVar ClientMonitors)
  }

data StatusMonitor = StatusMonitor {
  request     :: !ServerRequest
, monitor     :: !(TVar (RefCounted ResourceStatus))
, oldStatus   :: !(Maybe ResourceStatus) -- Nothing when added so we get a notification in any case.
}

data Snapshot = Snapshot {
  snapshotCurrent :: ResourceStatus
, fullMonitor     :: StatusMonitor
}
