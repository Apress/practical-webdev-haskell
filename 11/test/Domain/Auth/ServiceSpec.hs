module Domain.Auth.ServiceSpec where

import ClassyPrelude
import Domain.Auth.Types hiding (AuthService(..))
import Domain.Auth.Service
import Test.Hspec
import Fixture
import Katip

data Fixture m = Fixture
  { _addAuth :: Auth -> m (Either RegistrationError (UserId, VerificationCode))
  , _setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationError (UserId, Email))
  , _findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
  , _findEmailFromUserId :: UserId -> m (Maybe Email)
  , _notifyEmailVerification :: Email -> VerificationCode -> m ()
  , _newSession :: UserId -> m SessionId
  , _findUserIdBySessionId :: SessionId -> m (Maybe UserId)
  }

emptyFixture :: Fixture m
emptyFixture = Fixture
  { _addAuth = const unimplemented
  , _setEmailAsVerified = const unimplemented
  , _findUserByAuth = const unimplemented
  , _findEmailFromUserId = const unimplemented
  , _notifyEmailVerification = \_ _ -> unimplemented
  , _newSession = const unimplemented
  , _findUserIdBySessionId = const unimplemented
  }

newtype App a = App
  { unApp :: ReaderT (Fixture IO) (KatipContextT IO) a
  } deriving ( Applicative, Functor, Monad, MonadReader (Fixture IO), MonadIO
             , KatipContext, Katip
             )

runApp :: Fixture IO -> App a -> IO a
runApp fixture action = do
  le <- initLogEnv "HAuth" "test"
  runKatipContextT le () mempty . flip runReaderT fixture . unApp $ action

instance AuthRepo App where
  addAuth = dispatch _addAuth
  setEmailAsVerified = dispatch _setEmailAsVerified
  findUserByAuth = dispatch _findUserByAuth
  findEmailFromUserId = dispatch _findEmailFromUserId

instance EmailVerificationNotif App where
  notifyEmailVerification = dispatch2 _notifyEmailVerification

instance SessionRepo App where
  newSession = dispatch _newSession
  findUserIdBySessionId = dispatch _findUserIdBySessionId

spec :: Spec
spec = do

  let auth = either (error . show) id $ Auth <$> mkEmail "abc@123.com" <*> mkPassword "abcDEF123"

  describe "register" $ do
    it "should notify email verification upon successful registration" $ do
      tvar <- newTVarIO Nothing
      let fixture = emptyFixture
            { _addAuth =
              \_ -> return $ Right (1, "vcode")
            , _notifyEmailVerification =
              \email vCode -> atomically . writeTVar tvar $ Just (email, vCode)
            }
      runApp fixture (register auth) `shouldReturn` Right ()
      readTVarIO tvar `shouldReturn` Just (authEmail auth, "vcode")

    it "should return failure and not notify any email verification if adding to repo fail" $ do
      let fixture = emptyFixture
            { _addAuth = \_ -> return $ Left RegistrationErrorEmailTaken
            }
      runApp fixture (register auth)
        `shouldReturn` Left RegistrationErrorEmailTaken
      -- the fact that no exception thrown means no email verification is triggered

  describe "verifyEmail" $ do
    it "should call the correct repo function" $ do
      let fixture = emptyFixture
            { _setEmailAsVerified = \vcode -> case vcode of
                "vCode" -> return $ Right (1, authEmail auth)
                _ -> unimplemented
            }
      runApp fixture (verifyEmail "vCode")
        `shouldReturn` Right ()

    it "should return the failure if writing to repo fail" $ do
      let fixture = emptyFixture
            { _setEmailAsVerified =
              \_ -> return $ Left EmailVerificationErrorInvalidCode
            }
      runApp fixture (verifyEmail "vCode")
        `shouldReturn` Left EmailVerificationErrorInvalidCode

  describe "login" $ do
    it "should fail if the auth is incorrect" $ do
      let fixture = emptyFixture
            { _findUserByAuth =
              \_ -> return Nothing
            }
      runApp fixture (login auth)
        `shouldReturn` Left LoginErrorInvalidAuth
    it "should fail if the email has not been verified" $ do
      let fixture = emptyFixture
            { _findUserByAuth =
              \_ -> return $ Just (1, False)
            }
      runApp fixture (login auth)
        `shouldReturn` Left LoginErrorEmailNotVerified
    it "should return a session if the login is successful" $ do
      let fixture = emptyFixture
            { _findUserByAuth =
              \_ -> return $ Just (1, True)
            , _newSession =
              \uId -> if uId == 1 then return "sId" else unimplemented
            }
      runApp fixture (login auth)
        `shouldReturn` Right "sId"

  describe "resolveSessionId" $ do
    it "should return Nothing if the session is not found" $ do
      let fixture = emptyFixture
            { _findUserIdBySessionId =
              \_ -> return Nothing
            }
      runApp fixture (resolveSessionId "sId")
        `shouldReturn` Nothing
    it "should return UserId if the session is found" $ do
      let fixture = emptyFixture
            { _findUserIdBySessionId =
              \sId -> if sId == "sId" then return (Just 1) else unimplemented
            }
      runApp fixture (resolveSessionId "sId")
        `shouldReturn` Just 1

  describe "getUser" $ do
    it "should return Nothing if the user is not found" $ do
      let fixture = emptyFixture
            { _findEmailFromUserId =
              \_ -> return Nothing
            }
      runApp fixture (getUser 1)
        `shouldReturn` Nothing
    it "should return Email if the user is found" $ do
      let expected = Just (authEmail auth)
      let fixture = emptyFixture
            { _findEmailFromUserId =
              \uId -> if uId == 1 
                then return expected
                else unimplemented
            }
      runApp fixture (getUser 1)
        `shouldReturn` expected