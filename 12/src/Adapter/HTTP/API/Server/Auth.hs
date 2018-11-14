module Adapter.HTTP.API.Server.Auth where

import ClassyPrelude
import Web.Scotty.Trans
import Domain.Auth.Types
import qualified Text.Digestive.Form as DF
import Text.Digestive.Form ((.:))
import Adapter.HTTP.Common
import Adapter.HTTP.API.Server.Common
import Adapter.HTTP.API.Types.Auth ()
import Network.HTTP.Types.Status
import Katip

-- * Routes

routes :: ( ScottyError e, MonadIO m, KatipContext m, AuthService m)
          => ScottyT e m ()
routes = do
  -- register
  post "/api/auth/register" $ do
    input <- parseAndValidateJSON authForm
    domainResult <- lift $ register input
    case domainResult of
      Left err -> do
        status status400
        json err
      Right _ ->
        return ()

  -- verify email
  post "/api/auth/verifyEmail" $ do
    input <- parseAndValidateJSON verifyEmailForm
    domainResult <- lift $ verifyEmail input
    case domainResult of
      Left err -> do
        status status400
        json err
      Right _ ->
        return ()

  -- login
  post "/api/auth/login" $ do
    input <- parseAndValidateJSON authForm
    domainResult <- lift $ login input
    case domainResult of
      Left err -> do
        status status400
        json err
      Right sId -> do
        setSessionIdInCookie sId
        return ()

  -- get user
  get "/api/users" $ do
    userId <- reqCurrentUserId
    mayEmail <- lift $ getUser userId
    case mayEmail of
      Nothing ->
        raise $ stringError "Should not happen: SessionId map to invalid UserId"
      Just email ->
        json email

-- * Forms

verifyEmailForm :: (Monad m) => DF.Form [Text] m VerificationCode
verifyEmailForm = DF.text Nothing

authForm :: (Monad m) => DF.Form [Text] m Auth
authForm =
  Auth <$> "email" .: emailForm
       <*> "password" .: passwordForm
  where
    emailForm = DF.validate (toResult . mkEmail) (DF.text Nothing)
    passwordForm = DF.validate (toResult . mkPassword) (DF.text Nothing)