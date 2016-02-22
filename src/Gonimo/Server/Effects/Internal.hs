module Gonimo.Server.Effects.Internal (
  EServer
  , Server(..)
  , ServerException(..)
  , ServerConstraint
  , sendServer
  ) where

import Control.Exception.Base (SomeException)
import Control.Monad.Freer (send, Member, Eff)
import Control.Monad.Freer.Exception (throwError, Exc(..))
import Control.Monad.Logger (Loc, LogLevel, LogSource, ToLogStr)
import Data.ByteString (ByteString)
import Data.Text (Text)


import Data.Time.Clock (UTCTime)
import Database.Persist.Class (PersistStore)
import GHC.Generics

import Network.Mail.Mime (Mail)
import Gonimo.Database.Effects

-- Tidy up the following Server definition
type EServer a =  Server (Either ServerException a)

data Server v where
  SendEmail :: !Mail -> EServer ()
  LogMessage :: ToLogStr msg => Loc -> LogSource -> LogLevel -> msg -> EServer ()
  GenRandomBytes :: !Int -> EServer ByteString
  GetCurrentTime :: EServer UTCTime
  RunDb :: PersistStore backend => Eff '[Exc DbException, Database backend]  a -> EServer a

data ServerException =
    NotFoundException Text
  | SystemException SomeException deriving (Show, Generic)


-- Type synonym for constraints on Server API functions, requires ConstraintKinds language extension:
type ServerConstraint r = (Member Server r, Member (Exc ServerException) r)


-- Send a server operation, that is an operation that might fail:
sendServer :: ServerConstraint r => EServer a -> Eff r a
sendServer op = do
  r <- send op
  case r of
    Left e -> throwError e
    Right v -> return v