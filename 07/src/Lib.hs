module Lib
  ( main
  ) where

import ClassyPrelude
import qualified Adapter.RabbitMQ.Common as MQ
import qualified Adapter.RabbitMQ.Auth as MQAuth
import qualified Adapter.InMemory.Auth as M
import qualified Adapter.PostgreSQL.Auth as PG
import qualified Adapter.Redis.Auth as Redis
import qualified Adapter.HTTP.Main as HTTP
import Domain.Auth
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

instance AuthRepo App where
  addAuth = PG.addAuth
  setEmailAsVerified = PG.setEmailAsVerified
  findUserByAuth = PG.findUserByAuth
  findEmailFromUserId = PG.findEmailFromUserId

instance EmailVerificationNotif App where
  notifyEmailVerification = MQAuth.notifyEmailVerification

instance SessionRepo App where
  newSession = Redis.newSession
  findUserIdBySessionId = Redis.findUserIdBySessionId
  
withKatip :: (LogEnv -> IO a) -> IO a
withKatip =
  bracket createLogEnv closeScribes
  where
    createLogEnv = do
      logEnv <- initLogEnv "HAuth" "prod"
      stdoutScribe <- mkHandleScribe ColorIfTerminal stdout InfoS V2
      registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv

withState :: (Int -> LogEnv -> State -> IO ()) -> IO ()
withState action = 
  withKatip $ \le -> do
    mState <- newTVarIO M.initialState
    PG.withState pgCfg $ \pgState ->
      Redis.withState redisCfg $ \redisState ->
        MQ.withState mqCfg 16 $ \mqState -> do
          let state = (pgState, redisState, mqState, mState)
          action port le state
  where
    mqCfg = "amqp://guest:guest@localhost:5672/%2F"
    redisCfg = "redis://localhost:6379/0"
    pgCfg = 
      PG.Config 
      { PG.configUrl = "postgresql://localhost/hauth"
      , PG.configStripeCount = 2
      , PG.configMaxOpenConnPerStripe = 5
      , PG.configIdleConnTimeout = 10
      }
    port = 3000

main :: IO ()
main = 
  withState $ \port le state@(_, _, mqState, _) -> do
    let runner = run le state
    MQAuth.init mqState runner
    HTTP.main port runner
