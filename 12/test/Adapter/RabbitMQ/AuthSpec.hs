module Adapter.RabbitMQ.AuthSpec (spec) where

import ClassyPrelude
import Test.Hspec
import qualified Domain.Auth.Types as D
import qualified Adapter.RabbitMQ.Common as MQ
import qualified Adapter.RabbitMQ.Auth as MQ
import Katip
import Fixture
import System.Process

newtype Fixture m = Fixture
  { _sendEmailVerification :: D.Email -> D.VerificationCode -> m ()
  }

newtype App a = App
  { unApp :: ReaderT (Fixture IO) (KatipContextT IO) a
  } deriving ( Applicative, Functor, Monad, MonadIO, MonadReader (Fixture IO)
             , Katip, KatipContext, MonadThrow, MonadCatch
             )

instance MQ.EmailVerificationSender App where
  sendEmailVerification = dispatch2 _sendEmailVerification

runConsumerTestApp :: Fixture IO -> App a -> IO a
runConsumerTestApp fixture action = do
  le <- initLogEnv "HAuth" "test"
  runKatipContextT le () mempty . flip runReaderT fixture . unApp $ action

runPublisherTestApp :: MQ.State -> ReaderT MQ.State m a -> m a
runPublisherTestApp = flip runReaderT

spec :: Spec
spec = beforeAll initMQ $
  it "send and consume email verification notification should work" $
    MQ.withState testConf $ \mqState -> do
      mvar <- newEmptyMVar
      -- run consumer
      let fixture = Fixture
            { _sendEmailVerification =
                \mail code -> liftIO $ putMVar mvar (mail, code)
            }
      MQ.init mqState (runConsumerTestApp fixture)
      -- publish msg
      let email = either (error . show) id $ D.mkEmail "ecky@test.com"
          vCode = "vCode"
      runPublisherTestApp mqState $ MQ.notifyEmailVerification email vCode
      -- wait till the msg come
      takeMVar mvar `shouldReturn` (email, vCode)

initMQ :: IO ()
initMQ = do
  void  $ readProcessWithExitCode "rabbitmqctl" 
        [ "delete_vhost", "hauth_test_auth"
        ] ""
  void  $ readProcessWithExitCode "rabbitmqctl"
        [ "add_vhost", "hauth_test_auth"
        ] ""
  void  $ readProcessWithExitCode "rabbitmqctl"
        [ "set_permissions", "-p", "hauth_test_auth", "guest"
        , ".*", ".*", ".*"
        ] ""

testConf :: MQ.Config
testConf =
  MQ.Config { MQ.configUrl = "amqp://guest:guest@localhost:5672/hauth_test_auth"
            , MQ.configPrefetchCount = 8
            }
