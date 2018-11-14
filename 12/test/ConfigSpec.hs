module ConfigSpec (spec) where

import ClassyPrelude
import Test.Hspec
import System.Environment
import Config
import qualified Adapter.PostgreSQL.Auth as PG
import qualified Adapter.RabbitMQ.Common as MQ

spec :: Spec
spec = before initEnv $ do
  it "should fail if PORT is missing" $ do
    unsetEnv "PORT"
    void fromEnv `shouldThrow` anyException
  it "should fail if PORT is not a number" $ do
    setEnv "PORT" "NOT A NUMBER"
    void fromEnv `shouldThrow` anyException
  it "should fail if REDIS_URL is missing" $ do
    unsetEnv "REDIS_URL"
    void fromEnv `shouldThrow` anyException
  it "should fail if MQ_URL is missing" $ do
    unsetEnv "MQ_URL"
    void fromEnv `shouldThrow` anyException
  it "should fail if PG_URL is missing" $ do
    unsetEnv "PG_URL"
    void fromEnv `shouldThrow` anyException
  it "should parse config correctly" $
    fromEnv `shouldReturn` Config
      { configPort = 1234
      , configRedis = "REDIS_URL"
      , configMQ = MQ.Config "MQ_URL" 16
      , configPG = PG.Config "PG_URL" 2 5 10
      }

initEnv :: IO ()
initEnv = do
  setEnv "PORT" "1234"
  setEnv "REDIS_URL" "REDIS_URL"
  setEnv "MQ_URL" "MQ_URL"
  setEnv "PG_URL" "PG_URL"