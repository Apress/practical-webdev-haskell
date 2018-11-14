module Adapter.PostgreSQL.AuthSpec where

import ClassyPrelude
import Test.Hspec
import qualified Domain.Auth.Types as D
import Database.PostgreSQL.Simple
import Adapter.PostgreSQL.Auth
import Text.StringRandom

spec :: Spec
spec = beforeAll initDB $ do
  describe "addAuth" $
    it "should return email taken if the email already exists" $ do
      auth <- randomAuth
      runTestApp (addAuth auth >> addAuth auth)
        `shouldReturn` Left D.RegistrationErrorEmailTaken

  describe "setEmailAsVerified" $
    it "should return invalid code if the code is invalid" $
      runTestApp (setEmailAsVerified "invalidCode")
        `shouldReturn` Left D.EmailVerificationErrorInvalidCode

  describe "findUserByAuth" $ do
    it "should return Nothing if matching auth not found" $ do
      auth <- randomAuth
      runTestApp (findUserByAuth auth)
        `shouldReturn` Nothing
    it "should find the user (email has been verified) if matching auth found" $ do
      auth <- randomAuth
      runTestApp $ do
        Right (uId, vCode) <- addAuth auth
        void $ setEmailAsVerified vCode
        val <- findUserByAuth auth
        liftIO $ val `shouldBe` Just (uId, True)
    it "should find the user (email not yet verified) if matching auth found" $ do
      auth <- randomAuth
      runTestApp $ do
        Right (uId, _) <- addAuth auth
        val <- findUserByAuth auth
        liftIO $ val `shouldBe` Just (uId, False)

  describe "findEmailFromUserId" $ do
    it "should return Nothing if user id is not found" $
      runTestApp (findEmailFromUserId 0)
        `shouldReturn` Nothing
    it "should return correct email if user id is found" $ do
      auth <- randomAuth
      runTestApp $ do
        Right (uId, _) <- addAuth auth
        mayEmail <- findEmailFromUserId uId 
        liftIO $ mayEmail `shouldBe` Just (D.authEmail auth)

initDB :: IO ()
initDB = do
  conn <- connectPostgreSQL "postgresql://localhost"
  void $ execute_ conn "drop database if exists hauth_test_auth"
  void $ execute_ conn "create database hauth_test_auth"
  close conn
  withState testConf (const $ return ())

testConf :: Config
testConf =
  Config  { configUrl = "postgresql://localhost/hauth_test_auth"
          , configStripeCount = 2
          , configMaxOpenConnPerStripe = 5
          , configIdleConnTimeout = 10
          } 

runTestApp :: ReaderT State IO a -> IO a
runTestApp action =
  withPool testConf $ runReaderT action

randomAuth :: IO D.Auth
randomAuth = do
  email <- stringRandomIO "[A-Za-z0-9]{16}@test\\.com"
  return
    $ either (error . show) id
    $ D.Auth <$> D.mkEmail email <*> D.mkPassword "abcDEF123"