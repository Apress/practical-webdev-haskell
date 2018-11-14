module Adapter.HTTP.API.Auth where

import ClassyPrelude
import Web.Scotty.Trans
import Domain.Auth
import qualified Text.Digestive.Form as DF
import Text.Digestive.Form ((.:))
import Adapter.HTTP.Common
import Network.HTTP.Types.Status
import Data.Aeson ()
import Katip

-- * Routes

routes :: ( ScottyError e, MonadIO m, KatipContext m, AuthRepo m
          , EmailVerificationNotif m, SessionRepo m)
          => ScottyT e m ()
routes = do
  -- register
  post "/api/auth/register" $ do
    input <- parseAndValidateJSON authForm
    domainResult <- lift $ register input
    case domainResult of
      Left RegistrationErrorEmailTaken -> do
        status status400
        json ("EmailTaken" :: Text)
      Right _ ->
        return ()

  -- verify email
  post "/api/auth/verifyEmail" $ do
    input <- parseAndValidateJSON verifyEmailForm
    domainResult <- lift $ verifyEmail input
    case domainResult of
      Left EmailVerificationErrorInvalidCode -> do
        status status400
        json ("InvalidCode" :: Text)
      Right _ ->
        return ()

  -- login
  post "/api/auth/login" $ do
    input <- parseAndValidateJSON authForm
    domainResult <- lift $ login input
    case domainResult of
      Left LoginErrorInvalidAuth -> do
        status status400
        json ("InvalidAuth" :: Text)
      Left LoginErrorEmailNotVerified -> do
        status status400
        json ("EmailNotVerified" :: Text)
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
        json $ rawEmail email

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