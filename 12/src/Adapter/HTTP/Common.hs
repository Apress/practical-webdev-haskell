module Adapter.HTTP.Common where

import ClassyPrelude
import Web.Scotty.Trans
import Blaze.ByteString.Builder (toLazyByteString)
import Web.Cookie
import Domain.Auth.Types
import Data.Time.Lens
import qualified Text.Digestive.Types as DF

-- * Forms

toResult :: Either e a -> DF.Result e a
toResult = either DF.Error DF.Success

-- * Cookies

setCookie :: (Monad m) => SetCookie -> ActionT e m ()
setCookie = setHeader "Set-Cookie" . decodeUtf8 . toLazyByteString . renderSetCookie

getCookie :: (ScottyError e, Monad m) => Text -> ActionT e m (Maybe Text)
getCookie key = do
  mCookieStr <- header "Cookie"
  return $ do
    cookie <- parseCookies . encodeUtf8 . toStrict <$> mCookieStr
    let bsKey = encodeUtf8 key
    val <- lookup bsKey cookie
    return $ decodeUtf8 val

-- * Sessions

setSessionIdInCookie :: (MonadIO m, ScottyError e) => SessionId -> ActionT e m ()
setSessionIdInCookie sId = do
  curTime <- liftIO getCurrentTime
  setCookie $ def { setCookieName = "sId"
                  , setCookiePath = Just "/"
                  , setCookieValue = encodeUtf8 sId
                  , setCookieExpires = Just $ modL month (+ 1) curTime
                  , setCookieHttpOnly = True
                  , setCookieSecure = False
                  , setCookieSameSite = Just sameSiteLax
                  }

getCurrentUserId :: (AuthService m, ScottyError e) => ActionT e m (Maybe UserId)
getCurrentUserId = do
  maySessionId <- getCookie "sId"
  case maySessionId of
    Nothing -> return Nothing
    Just sId -> lift $ resolveSessionId sId