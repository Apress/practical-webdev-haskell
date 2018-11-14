module Config where

import ClassyPrelude
import System.Environment
import qualified Adapter.PostgreSQL.Auth as PG
import qualified Adapter.RabbitMQ.Common as MQ

data Config = Config
  { configPort :: Int
  , configRedis :: String
  , configMQ :: MQ.Config
  , configPG :: PG.Config
  }

fromEnv :: IO Config
fromEnv = Config
  <$> envRead "PORT"
  <*> getEnv "REDIS_URL"
  <*> (MQ.Config
        <$> getEnv "MQ_URL"
        <*> pure 16
      )
  <*> (PG.Config
        <$> envFromString "PG_URL"
        <*> pure 2
        <*> pure 5
        <*> pure 10
      )

devConfig :: Config
devConfig = Config
  { configPort = 3000
  , configRedis = "redis://localhost:6379/0"
  , configMQ = MQ.Config
    { MQ.configUrl = "amqp://guest:guest@localhost:5672/%2F"
    , MQ.configPrefetchCount = 16
    }
  , configPG = PG.Config
    { PG.configUrl = "postgresql://localhost/hauth"
    , PG.configStripeCount = 2
    , PG.configMaxOpenConnPerStripe = 5
    , PG.configIdleConnTimeout = 10
    }
  }

-- * Helpers

envFromString :: (IsString a) => String -> IO a
envFromString key = fromString <$> getEnv key

envRead :: Read a => String -> IO a
envRead key = do
  rawVal <- getEnv key
  case readMay rawVal of
    Just val -> return val
    Nothing -> throwString $ key <> ": Unable to parse " <> rawVal
