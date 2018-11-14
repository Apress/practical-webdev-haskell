module Adapter.HTTP.Fixture where

import ClassyPrelude
import Domain.Auth.Types
import Katip
import Network.Wai
import qualified Adapter.HTTP.Main as HTTP
import Fixture

data Fixture m = Fixture
  { _register :: Auth -> m (Either RegistrationError ())
  , _verifyEmail :: VerificationCode -> m (Either EmailVerificationError ())
  , _login :: Auth -> m (Either LoginError SessionId)
  , _resolveSessionId :: SessionId -> m (Maybe UserId)
  , _getUser :: UserId -> m (Maybe Email)
  }

emptyFixture :: Fixture IO
emptyFixture = Fixture
  { _register = const unimplemented
  , _verifyEmail = const unimplemented
  , _login = const unimplemented
  , _resolveSessionId = const unimplemented
  , _getUser = const unimplemented
  }

newtype App a = App 
  { unApp :: ReaderT (Fixture IO) (KatipContextT IO) a
  } deriving ( Applicative, Functor, Monad, MonadReader (Fixture IO), MonadIO
             , KatipContext, Katip
             )

app :: Fixture IO -> IO Application
app fixture = do
  le <- initLogEnv "HAuth" "test"
  let runner = runKatipContextT le () mempty . flip runReaderT fixture . unApp
  HTTP.app runner

instance AuthService App where
  register = dispatch _register
  verifyEmail = dispatch _verifyEmail
  login = dispatch _login
  resolveSessionId = dispatch _resolveSessionId
  getUser = dispatch _getUser