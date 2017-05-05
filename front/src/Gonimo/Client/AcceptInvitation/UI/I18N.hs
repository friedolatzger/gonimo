{-# LANGUAGE OverloadedStrings #-}
module Gonimo.Client.AcceptInvitation.UI.I18N where

import           Gonimo.I18N
import Data.Text (Text)
import Data.Monoid ((<>))

data Message = Family_Invitation
             | The_device_invited_you_to_join Text Text
             | Inviting_User
             | Decline
             | Accept
             | This_generous_offer
             deriving (Show, Eq)

instance I18N Message where
  i18n EN_GB Family_Invitation = "Family Invitation"
  i18n DE_DE Family_Invitation = "Familieneinladung"
  i18n EN_GB (The_device_invited_you_to_join deviceName familyName)
    = "The device '" <> deviceName <> "' invited you to join family '" <> familyName <> "'!"
  i18n DE_DE  (The_device_invited_you_to_join deviceName familyName)
    = "Das Gerät '" <> deviceName <> "' hat dir eine Einladung für die Familie '" <> familyName <> "' gesendet!"
  i18n EN_GB Inviting_User = "Inviting User:"
  i18n DE_DE Inviting_User = "Einladung von:"
  i18n EN_GB Decline = "Decline"
  i18n DE_DE Decline = "Ablehnen"
  i18n EN_GB Accept = "Accept"
  i18n DE_DE Accept = "Akzeptiere"
  i18n EN_GB This_generous_offer = " this generous offer "
  i18n DE_DE This_generous_offer = " dieses großzügigies Angebot "
