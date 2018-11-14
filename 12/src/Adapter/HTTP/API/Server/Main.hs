module Adapter.HTTP.API.Server.Main where

import ClassyPrelude
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import qualified Adapter.HTTP.API.Server.Auth as Auth
import Domain.Auth.Types
import Katip
import Network.Wai
import Network.Wai.Middleware.Gzip

main :: ( MonadIO m, KatipContext m, AuthService m)
     => (m Response -> IO Response) -> IO Application
main runner =
  scottyAppT runner routes

routes :: ( MonadIO m, KatipContext m, AuthService m)
       => ScottyT LText m ()
routes = do
  middleware $ gzip $ def { gzipFiles = GzipCompress }

  Auth.routes

  notFound $ do
    status status404
    json ("NotFound" :: Text)

  defaultHandler $ \e -> do
    lift $ $(logTM) ErrorS $ "Unhandled error: " <> ls (showError e)
    status status500
    json ("InternalServerError" :: Text)
