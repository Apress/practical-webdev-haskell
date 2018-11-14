module Lib
  ( main
  , mainDev
  ) where

import ClassyPrelude
import qualified Adapter.RabbitMQ.Common as MQ
import qualified Adapter.RabbitMQ.Auth as MQAuth
import qualified Adapter.InMemory.Auth as M
import qualified Adapter.PostgreSQL.Auth as PG
import qualified Adapter.Redis.Auth as Redis
import qualified Adapter.HTTP.Main as HTTP
import qualified Config
import Domain.Auth.Types
import qualified Domain.Auth.Service as D
import Katip

type State = (PG.State, Redis.State, MQ.State, TVar M.State)

newtype App a = App
  { unApp :: ReaderT State (KatipContextT IO) a
  } deriving ( Applicative, Functor, Monad, MonadReader State, MonadIO
             , KatipContext, Katip, MonadThrow, MonadCatch)

run :: LogEnv -> State -> App a -> IO a
run le state 
  = runKatipContextT le () mempty
  . flip runReaderT state 
  . unApp

instance D.AuthRepo App where
  addAuth = PG.addAuth
  setEmailAsVerified = PG.setEmailAsVerified
  findUserByAuth = PG.findUserByAuth
  findEmailFromUserId = PG.findEmailFromUserId

instance D.EmailVerificationNotif App where
  notifyEmailVerification = MQAuth.notifyEmailVerification

instance D.SessionRepo App where
  newSession = Redis.newSession
  findUserIdBySessionId = Redis.findUserIdBySessionId

instance AuthService App where
  register = D.register
  verifyEmail = D.verifyEmail
  login = D.login
  resolveSessionId = D.resolveSessionId
  getUser = D.getUser

instance MQAuth.EmailVerificationSender App where
  sendEmailVerification = M.notifyEmailVerification
  
withKatip :: (LogEnv -> IO a) -> IO a
withKatip =
  bracket createLogEnv closeScribes
  where
    createLogEnv = do
      logEnv <- initLogEnv "HAuth" "prod"
      stdoutScribe <- mkHandleScribe ColorIfTerminal stdout InfoS V2
      registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv

withState :: Config.Config -> (Int -> LogEnv -> State -> IO ()) -> IO ()
withState config action =
  withKatip $ \le -> do
    mState <- newTVarIO M.initialState
    PG.withState (Config.configPG config) $ \pgState ->
      Redis.withState (Config.configRedis config) $ \redisState ->
        MQ.withState (Config.configMQ config) $ \mqState -> do
          let state = (pgState, redisState, mqState, mState)
          action (Config.configPort config) le state

mainWithConfig :: Config.Config -> IO ()
mainWithConfig config = 
  withState config $ \port le state@(_, _, mqState, _) -> do
    let runner = run le state
    MQAuth.init mqState runner
    HTTP.main port runner

main :: IO ()
main = do
  config <- Config.fromEnv
  mainWithConfig config

mainDev :: IO ()
mainDev = mainWithConfig Config.devConfig
