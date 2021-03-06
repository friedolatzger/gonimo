{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}

module Gonimo.Client.WebRTC.Channel where

import Gonimo.Client.Prelude

import           Control.Lens
import           Control.Monad.Reader.Class
import           GHCJS.DOM.EventM
import           GHCJS.DOM.Enums                (MediaStreamTrackState(..))
import           GHCJS.DOM.RTCIceCandidate
import           GHCJS.DOM.RTCIceCandidateEvent as IceEvent
-- #ifdef __GHCJS__
import           GHCJS.DOM.RTCPeerConnection    as RTCPeerConnection hiding (newRTCPeerConnection)
-- #else
-- import           JSDOM.Custom.RTCPeerConnection  as RTCPeerConnection hiding (newRTCPeerConnection)
-- -- import           JSDOM.Generated.RTCPeerConnection  as RTCPeerConnection (addStream)
-- #endif
import           Gonimo.Db.Entities             (DeviceId)
import qualified Gonimo.SocketAPI               as API
import qualified Gonimo.SocketAPI.Types         as API
import           Gonimo.Types                   (Secret)
import           Reflex.Dom.Core

import           GHCJS.DOM.Types                (Dictionary (..), MediaStream,
                                                 MonadJSM, RTCPeerConnection)
import           Gonimo.Client.Config
import           Gonimo.DOM.Window              (newRTCPeerConnection)

import           Data.Maybe
import qualified GHCJS.DOM.MediaStream          as MediaStream
import           GHCJS.DOM.MediaStreamTrack     (ended, getReadyState)
import           Language.Javascript.JSaddle    (liftJSM, (<#))
import qualified Language.Javascript.JSaddle    as JS
import           Safe                           (fromJustNote)

data CloseEvent = CloseRequested | CloseConnectionLoss
type Message = API.Message

data ChannelEvent = ChannelEvent (API.FromId, Secret) RTCEvent
data RTCEvent
  = RTCEventGotRemoteStream !MediaStream
  | RTCEventNegotiationNeeded
  | RTCEventIceCandidate !RTCIceCandidate
  | RTCEventConnectionClosed
  | RTCEventRemoteStreamEnded

data ReceivingState
  = StateNotReceiving
  | StateUnreliable -- We got a stream but did not get any stats for more than a few seconds!
  | StateReceiving Int -- All is fine - we are receiving stats, contains number of uninterrupted zero package count updates.
  deriving (Eq, Ord, Show)

-- If StateReceiving counter is equal or above this number a connection can be considered broken (alarm should be triggered)
considerBrokenCount :: Int
considerBrokenCount = 4

isStateBroken :: ReceivingState -> Bool
isStateBroken (StateReceiving n) = n >= considerBrokenCount
isStateBroken _ = False

data Config t
  = Config  { _configResponse :: Event t API.ServerResponse
            , _configTriggerChannelEvent :: ChannelEvent -> IO ()
            , _configTheirId :: DeviceId
            , _configSecret :: Secret
            }

data Channel t
  = Channel { _rtcConnection :: RTCPeerConnection
            , _theirStream :: Maybe MediaStream
            , _closeRequested :: Bool -- Signal that a close is requested (don't trigger alarm when connection gets closed)
            , _audioReceivingState :: ReceivingState
            , _videoReceivingState :: ReceivingState
            , _audioMuted :: Bool
            , _videoMuted :: Bool
            }

makeLenses ''Config
makeLenses ''Channel

channel :: forall m t. (MonadJSM m, Reflex t) => Config t -> m (Channel t)
channel config = mdo
  conn <- makeGonimoRTCConnection

  handleRTCClosedEvent config conn
  handleReceivedStream config conn
  handleIceCandidate config conn
  handleNegotiationNeeded config conn

  pure $ Channel { _rtcConnection = conn
                 , _theirStream = Nothing
                 , _closeRequested = False
                 , _audioReceivingState = StateNotReceiving
                 , _videoReceivingState = StateNotReceiving
                 , _audioMuted = False
                 , _videoMuted = False
                 }

-- Get the worst state available for a channel.
worstState :: Channel t -> ReceivingState
worstState chan = if badness (chan^.audioReceivingState) > badness (chan^.videoReceivingState)
                  then chan^.audioReceivingState
                  else chan^.videoReceivingState
  where
    badness :: ReceivingState -> Int
    badness (StateReceiving n)
      | n < considerBrokenCount = 0
      | otherwise = 3
    badness StateNotReceiving = 1
    badness StateUnreliable = 2

needsAlert :: Channel t -> Bool
needsAlert chan = isStateBroken (chan^.audioReceivingState)
                  || isStateBroken (chan^.videoReceivingState)
                  || chan^.audioMuted
                  || chan^.videoMuted

-- Handle RTCPeerConnection close.
handleRTCClosedEvent :: forall m t. (MonadJSM m) => Config t -> RTCPeerConnection -> m ()
handleRTCClosedEvent config conn = liftJSM $ do
  let
    triggerCloseEv = config^.configTriggerChannelEvent
                     $ ChannelEvent (config^.configTheirId, config^.configSecret) RTCEventConnectionClosed
  listener <- newListener $ do
    state :: Text <- liftJSM $ getIceConnectionState conn
    if state == "closed"
      then liftIO $ triggerCloseEv
      else pure ()
  addListener conn iceConnectionStateChange listener False

-- Handle receiption of a remote stream (trigger channel event)
handleReceivedStream :: forall m t. (MonadJSM m)
                        => Config t -> RTCPeerConnection -> m ()
handleReceivedStream config conn = liftJSM $ do
  let mapKey = (config^.configTheirId, config^.configSecret)
  let triggerChannelEvent = config^.configTriggerChannelEvent
  let triggerRTCEvent = triggerChannelEvent
                        . ChannelEvent mapKey
                        . RTCEventGotRemoteStream
  listener <- newListener $ do
    e <- ask
    rawStream <- liftJSM $ (JS.toJSVal e) JS.! ("stream" :: Text)
    mStream <- liftJSM $ JS.fromJSVal rawStream
    let stream = fromJustNote "event had no valid MediaStream!" mStream

    tracks <- catMaybes <$> MediaStream.getTracks stream
    let sendStreamEnded = do
          cStates <- traverse getReadyState tracks
          liftIO . when (all (== MediaStreamTrackStateEnded) cStates) $
            triggerChannelEvent $ ChannelEvent mapKey RTCEventRemoteStreamEnded

    endedListener <- lift . newListener $ sendStreamEnded

    let addEndedListener (event', track) = liftJSM $ addListener track event' endedListener False
    traverse_ addEndedListener $ (ended,) <$> tracks
    sendStreamEnded

    liftIO . triggerRTCEvent $ stream

  addListener conn addStreamEvent listener False

handleNegotiationNeeded :: forall m t. (MonadJSM m)
                           => Config t -> RTCPeerConnection -> m ()
handleNegotiationNeeded config conn = liftJSM $ do
  let triggerRTCEvent = (config^.configTriggerChannelEvent)
                        . ChannelEvent (config^.configTheirId, config^.configSecret)
                        $ RTCEventNegotiationNeeded
  listener <- newListener . liftIO $ triggerRTCEvent
  addListener conn negotiationNeeded listener False

handleIceCandidate :: forall m t. (MonadJSM m)
                           => Config t -> RTCPeerConnection -> m ()
handleIceCandidate config conn = liftJSM $ do
  let triggerRTCEvent = (config^.configTriggerChannelEvent)
                        . ChannelEvent (config^.configTheirId, config^.configSecret)
                        . RTCEventIceCandidate
  listener <- newListener $ do
    e <- ask
    mCandidate <- IceEvent.getCandidate e
    liftIO $ traverse_ triggerRTCEvent mCandidate
  addListener conn iceCandidate listener False

makeGonimoRTCConnection :: MonadJSM m => m RTCPeerConnection
makeGonimoRTCConnection = liftJSM $ do
  config <- JS.obj
  config <# ("urls" :: Text) $ JS.toJSVal [gonimoTurnServer]
  config <# ("username" :: Text)$ JS.toJSVal gonimoTurnUser
  config <# ("credential" :: Text) $ JS.toJSVal gonimoTurnPassword
  config <# ("credentialType" :: Text) $ JS.toJSVal gonimoTurnCredentialType
  allServers <- JS.obj
  allServers <# ("iceServers" :: Text) $ [config]
  let configDic = case allServers of JS.Object val -> Dictionary val
  newRTCPeerConnection $ Just configDic

-- Don't use plain close, it throws uncatchable exceptions when connection is already closed:
safeClose :: MonadJSM m => RTCPeerConnection -> m ()
safeClose conn = liftJSM $ do
      jsClose <- JS.eval $ ("(function(conn) { try {conn.close();} catch(e) {console.log(\"Catched: \" + e.toString());}})" :: Text)
      _ <- JS.call jsClose JS.obj [conn]
      pure ()
