{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.Family.RoleSelector where

import           Reflex.Dom.Core

import           Gonimo.Client.Family.Internal
import           Gonimo.Client.Reflex.Dom
import           Gonimo.Client.Prelude
import           Data.Map (Map)



roleSelector :: forall t m. (MonadFix m, DomBuilder t m, MonadHold t m, PostBuild t m)
          => m (Event t GonimoRole, Event t ())
roleSelector = do
  elClass "div" "btn-box" $ do
    babyClicked <-
      makeClickable . elAttr' "div" (addFullScreenBtnAttrs "btn-baby") $ do
        elAttr "img" ("src" =: "/pix/button-baby.svg") blank
        el "span" $ text "BABY"
    parentClicked <-
      makeClickable . elAttr' "div" (addFullScreenBtnAttrs "btn-parent") $ do
        elAttr "img" ("src" =: "/pix/button-parent.svg") blank
        el "span" $ text "PARENT"
    inviteRequested <-
          makeClickable . elAttr' "div" (addBtnAttrs "device-add") $ text " Add Device"
    pure $ ( leftmost [ const RoleBaby <$> babyClicked
                      , const RoleParent <$> parentClicked
                      ]
           , inviteRequested
           )

addFullScreenBtnAttrs :: Text -> Map Text Text
addFullScreenBtnAttrs className
  = "class" =: className
  <> "type" =: "button"
  <> "role" =: "button"
  <> "onClick" =: "(function() {if (screenfull.enabled) {screenfull.request();}})()"
