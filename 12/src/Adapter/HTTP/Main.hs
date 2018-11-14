module Adapter.HTTP.Main where

import ClassyPrelude
import qualified Adapter.HTTP.API.Server.Main as API
import qualified Adapter.HTTP.Web.Main as Web
import Domain.Auth.Types
import Katip
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Vhost

app :: (KatipContext m, AuthService m)
    => (m Response -> IO Response) -> IO Application
app runner = do
  web <- Web.main runner
  api <- API.main runner
  return $ vhost [(pathBeginsWith "api", api)] web
  where
    pathBeginsWith path req = headMay (pathInfo req) == Just path

main :: (KatipContext m, AuthService m)
     => Int -> (m Response -> IO Response) -> IO ()
main port runner =
  app runner >>= run port
