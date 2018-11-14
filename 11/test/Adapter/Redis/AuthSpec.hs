module Adapter.Redis.AuthSpec where

import ClassyPrelude
import Test.Hspec
import qualified Database.Redis as R
import Adapter.Redis.Auth

spec :: Spec
spec = beforeAll initDB $
  describe "findUserIdBySessionId" $ do
    it "should return Nothing if session is invalid" $
      runTestApp (findUserIdBySessionId "invalidSession")
        `shouldReturn` Nothing
    it "should return valid user id if session is valid" $ do
      let uId = 1
      runTestApp (newSession uId >>= findUserIdBySessionId)
        `shouldReturn` Just uId

initDB :: IO ()
initDB = do
  let connInfo = either (error "Invalid Redis conn URL") id
               $ R.parseConnectInfo testConf
  conn <- R.checkedConnect connInfo
  void $ R.runRedis conn R.flushdb

testConf :: String
testConf = "redis://localhost:6379/8"

runTestApp :: ReaderT State IO a -> IO a
runTestApp action =
  withState testConf $ runReaderT action