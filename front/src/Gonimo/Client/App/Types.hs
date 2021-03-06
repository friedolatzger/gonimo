{-# LANGUAGE TemplateHaskell #-}
module Gonimo.Client.App.Types where

import Reflex
import qualified Gonimo.SocketAPI as API
import qualified Gonimo.SocketAPI.Types as API
import Control.Lens
import qualified Gonimo.Client.Server as Server
import qualified Gonimo.Client.Auth as Auth
import qualified Gonimo.Client.Subscriber as Subscriber
import Gonimo.Client.Subscriber (SubscriptionsDyn)
import qualified Gonimo.Db.Entities as Db
import Data.Map (Map)
import Control.Monad
import Gonimo.Client.Prelude
import qualified Gonimo.Types as Gonimo
import Data.Default
import qualified Data.Set as Set
import Gonimo.I18N

data Config t
  = Config { _server :: Server.Server t
           , _auth :: Auth.Auth t
           , _subscriber :: Subscriber.Subscriber t
           }

data Loaded t
  = Loaded { _authData :: Dynamic t API.AuthData
           , _families :: Dynamic t (Map Db.FamilyId Db.Family)
           , _selectedFamily :: Dynamic t Db.FamilyId
           }

data App t
  = App { _subscriptions :: SubscriptionsDyn t
        , _request :: Event t [ API.ServerRequest ]
        , _selectLang :: Event t Locale
        }

data Screen t
  = Screen { _screenApp :: App t
           , _screenGoHome :: Event t ()
           }

makeLenses ''Config
makeLenses ''Loaded
makeLenses ''App
makeLenses ''Screen


instance (Reflex t) => Default (App t) where
  def = App (constDyn Set.empty) never never

instance (Reflex t) => Default (Screen t) where
  def = Screen def never

appSwitchPromptlyDyn :: forall t. Reflex t => Dynamic t (App t) -> App t
appSwitchPromptlyDyn ev
  = App { _subscriptions = join $ _subscriptions <$> ev
        , _request = switchPromptlyDyn $ _request <$> ev
        , _selectLang = switchPromptlyDyn $ _selectLang <$> ev
        }

screenSwitchPromptlyDyn :: forall t. Reflex t => Dynamic t (Screen t) -> Screen t
screenSwitchPromptlyDyn ev
  = Screen { _screenApp = appSwitchPromptlyDyn (_screenApp <$> ev)
           , _screenGoHome = switchPromptlyDyn $ _screenGoHome <$> ev
           }

currentFamilyName :: forall t. Reflex t => Loaded t -> Dynamic t Text
currentFamilyName loaded =
    let
      getFamilyName :: Db.FamilyId -> Map Db.FamilyId Db.Family -> Text
      getFamilyName fid families' = families'^.at fid._Just.to Db.familyName . to Gonimo.familyName
    in
      zipDynWith getFamilyName (loaded^.selectedFamily) (loaded^.families)

babyNames :: forall t. Reflex t => Loaded t -> Dynamic t [Text]
babyNames loaded =
    let
      getBabyNames :: Db.FamilyId -> Map Db.FamilyId Db.Family -> [Text]
      getBabyNames fid families' = families'^.at fid._Just.to Db.familyLastUsedBabyNames
    in
      zipDynWith getBabyNames (loaded^.selectedFamily) (loaded^.families)
