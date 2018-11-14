module Adapter.HTTP.API.Client.Common where

import ClassyPrelude
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Has
import Data.Aeson

newtype Config = Config
  { configUrl :: String
  }

data State = State
  { stateInitReq :: Request
  , stateManager :: Manager
  }

type HttpClient r m = (MonadReader r m, Has State r, MonadIO m, MonadThrow m)

type Session = CookieJar

data UnexpectedResponse a =
  UnexpectedResponse Request (Response a) deriving (Show)

instance (Typeable a, Show a) => Exception (UnexpectedResponse a)

-- * Initialize

withState :: Config -> (State -> IO a) -> IO a
withState cfg action = do
  mgr <- newManager tlsManagerSettings
  initReq <- parseRequest $ configUrl cfg
  let initReqWithJson = 
        initReq { requestHeaders =
                    [("Content-Type", "application/json; charset=utf-8")]
                }
  action $ State initReqWithJson mgr

-- * Helpers

parseOrErr :: (MonadThrow m, FromJSON a)
           => Request -> Response LByteString -> m a
parseOrErr req resp =
  case eitherDecode' $ responseBody resp of
    Left _ -> throw $ UnexpectedResponse req resp
    Right a -> return a