module Adapter.HTTP.API.Common where

import ClassyPrelude
import Web.Scotty.Trans
import Domain.Auth
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Aeson as DF
import Data.Aeson hiding (json)
import Network.HTTP.Types.Status
import Adapter.HTTP.Common

-- * Forms

parseAndValidateJSON :: (ScottyError e, MonadIO m, ToJSON v)
                     => DF.Form v m a -> ActionT e m a
parseAndValidateJSON form = do
  val <- jsonData `rescue` (\_ -> return Null)
  validationResult <- lift $ DF.digestJSON form val
  case validationResult of
    (v, Nothing) -> do
      status status400
      json $ DF.jsonErrors v
      finish
    (_, Just result) ->
      return result

-- * Sessions

reqCurrentUserId :: (SessionRepo m, ScottyError e) => ActionT e m UserId
reqCurrentUserId = do
  mayUserId <- getCurrentUserId
  case mayUserId of
    Nothing -> do
      status status401
      json ("AuthRequired" :: Text)
      finish
    Just userId ->
      return userId