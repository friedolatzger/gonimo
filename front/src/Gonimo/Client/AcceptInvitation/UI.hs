{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.AcceptInvitation.UI where

import           Gonimo.Client.Prelude

import           Control.Lens
import           Control.Monad.Fix                       (MonadFix)
import           Control.Monad.Trans.Class               (lift)
import           Control.Monad.Trans.Maybe               (runMaybeT)
import           Data.Maybe                              (fromMaybe, maybe)
import           Data.Text                               (Text)
import           Gonimo.Client.AcceptInvitation.Internal
import qualified Gonimo.SocketAPI                        as API
import           Gonimo.SocketAPI.Types                  (InvitationInfo (..))
import           Gonimo.SocketAPI.Types                  (InvitationReply (..))
import           Gonimo.Types                            (Secret)
import qualified Gonimo.Types                            as Gonimo
import           Reflex.Dom.Core
import           GHCJS.DOM.Types (MonadJSM)
import qualified Data.Set                                as Set
import           Gonimo.Client.AcceptInvitation.UI.I18N
import           Gonimo.I18N
import           Gonimo.Client.Reflex.Dom

ui :: forall m t. GonimoM t m
      => Config t -> m (AcceptInvitation t)
ui config = fmap (fromMaybe emptyAcceptInvitation) . runMaybeT $ do
    secret <- getInvitationSecret
    clearInvitationFromURL
    answerReq <- lift . fmap switchPromptlyDyn -- Only display until user accepted/declined.
                 . widgetHold (onInvitationUI secret)
                 $ const (pure never) <$> gotAnswerResponse

    -- Make sure invitation gets claimed, by subscribing the request:
    claimSub <- lift . holdDyn (Set.singleton (API.ReqClaimInvitation secret)) $ const Set.empty <$> answerReq
    pure $ AcceptInvitation answerReq claimSub
  where
    onInvitationUI secret = fmap switchPromptlyDyn
                     . widgetHold (pure never)
                     $ ui' secret <$> gotInvitation
    gotInvitation = push (\res -> case res of
                            API.ResClaimedInvitation _ invInfo -> pure $ Just invInfo
                            _ -> pure Nothing
                        ) (config^.configResponse)
    gotAnswerResponse = push (\res -> case res of
                            API.ResAnsweredInvitation _ _ _ -> pure $ Just ()
                            API.ResError (API.ReqAnswerInvitation _ _) _ -> pure $ Just ()
                            _ -> pure Nothing
                        ) (config^.configResponse)

ui' :: forall m t. GonimoM t m
      => Secret -> InvitationInfo -> m (Event t [API.ServerRequest])
ui' secret invInfo = do
  elClass "div" "notification overlay" $
    elClass "div" "container" $
      elClass "div" "notification box" $ do
        elClass "div" "notification header" $
          el "h1" $ trText Family_Invitation
        elClass "notification" "text" $ do
          trText $ The_device_invited_you_to_join (invitationInfoSendingDevice invInfo) (Gonimo.familyName . invitationInfoFamily $ invInfo)
          -- flip (maybe (pure ())) (invitationInfoSendingUser invInfo) $ \invUser ->
          --   el "tr" $ do
          --     el "td" $ trText Inviting_User
          --     el "td" $ text invUser
          elClass "div" "notification btn-box" $ do
            accepted <- makeClickable . elAttr' "div" (addBtnAttrs "notification left-btn") $ do
              trText Accept
              -- elClass "span" "hidden-xs" $ trText This_generous_offer
              -- elClass "i" "fa fa-fw fa-check" blank
            declined <- makeClickable . elAttr' "div" (addBtnAttrs "notification right-btn") $ do
              trText Decline
              elClass "i" "fa fa-fw fa-times" blank
            pure $ mconcat [ makeAnswerInvitation secret . fmap (const InvitationReject) $ declined
                           , makeAnswerInvitation secret . fmap (const InvitationAccept) $ accepted
                           ]
