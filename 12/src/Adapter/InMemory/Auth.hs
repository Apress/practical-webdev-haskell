module Adapter.InMemory.Auth where
  
import ClassyPrelude
import qualified Domain.Auth.Types as D
import Control.Monad.Except
import Text.StringRandom
import Data.Has

data State = State
  { stateAuths :: [(D.UserId, D.Auth)]
  , stateUnverifiedEmails :: Map D.VerificationCode D.Email
  , stateVerifiedEmails :: Set D.Email
  , stateUserIdCounter :: Int
  , stateNotifications :: Map D.Email D.VerificationCode
  , stateSessions :: Map D.SessionId D.UserId
  } deriving (Show, Eq)

initialState :: State
initialState = State 
  { stateAuths = []
  , stateUnverifiedEmails = mempty
  , stateVerifiedEmails = mempty
  , stateUserIdCounter = 0
  , stateNotifications = mempty
  , stateSessions = mempty
  }

type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

addAuth :: InMemory r m
        => D.Auth -> m (Either D.RegistrationError (D.UserId, D.VerificationCode))
addAuth auth = do
  tvar <- asks getter

  -- gen verification code
  vCode <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}"

  atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    -- check whether the given email is duplicate
    let auths = stateAuths state
        email = D.authEmail auth
        isDuplicate = elem email . map (D.authEmail . snd) $ auths
    when isDuplicate $ throwError D.RegistrationErrorEmailTaken
    -- update the state
    let newUserId = stateUserIdCounter state + 1
        newAuths = (newUserId, auth) : auths
        unverifieds = stateUnverifiedEmails state
        newUnverifieds = insertMap vCode email unverifieds
        newState = state
          { stateAuths = newAuths
          , stateUserIdCounter = newUserId
          , stateUnverifiedEmails = newUnverifieds
          }
    lift $ writeTVar tvar newState
    return (newUserId, vCode)

setEmailAsVerified :: InMemory r m
                   => D.VerificationCode
                   -> m (Either D.EmailVerificationError (D.UserId, D.Email))
setEmailAsVerified vCode = do
  tvar <- asks getter
  atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    let unverifieds = stateUnverifiedEmails state
        mayEmail = lookup vCode unverifieds
    email <- mayEmail `orThrow` D.EmailVerificationErrorInvalidCode
    let auths = stateAuths state
        mayUserId = map fst . find ((email ==) . D.authEmail . snd) $ auths
    uId <- mayUserId `orThrow` D.EmailVerificationErrorInvalidCode
    let verifieds = stateVerifiedEmails state
        newVerifieds = insertSet email verifieds
        newUnverifieds = deleteMap vCode unverifieds
        newState = state 
          { stateUnverifiedEmails = newUnverifieds
          , stateVerifiedEmails = newVerifieds
          }
    lift $ writeTVar tvar newState
    return (uId, email)

findUserByAuth :: InMemory r m
               => D.Auth -> m (Maybe (D.UserId, Bool))
findUserByAuth auth = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  let mayUserId = map fst . find ((auth ==) . snd) $ stateAuths state
  case mayUserId of
    Nothing -> return Nothing
    Just uId -> do
      let verifieds = stateVerifiedEmails state
          email = D.authEmail auth
          isVerified = email `elem` verifieds
      return $ Just (uId, isVerified)

findEmailFromUserId :: InMemory r m
                    => D.UserId -> m (Maybe D.Email)
findEmailFromUserId uId = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  let mayAuth = map snd . find ((uId ==) . fst) $ stateAuths state
  return $ D.authEmail <$> mayAuth

notifyEmailVerification :: InMemory r m
                        => D.Email -> D.VerificationCode -> m ()
notifyEmailVerification email vCode = do
  tvar <- asks getter
  atomically $ do
    state <- readTVar tvar
    let notifications = stateNotifications state
        newNotifications = insertMap email vCode notifications
        newState = state { stateNotifications = newNotifications }
    writeTVar tvar newState

getNotificationsForEmail :: InMemory r m
                         => D.Email -> m (Maybe D.VerificationCode)
getNotificationsForEmail email = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  return $ lookup email $ stateNotifications state

newSession :: InMemory r m
           => D.UserId -> m D.SessionId
newSession uId = do
  tvar <- asks getter
  sId <- liftIO $ (tshow uId <>) <$> stringRandomIO "[A-Za-z0-9]{16}"
  atomically $ do
    state <- readTVar tvar
    let sessions = stateSessions state
        newSessions = insertMap sId uId sessions
        newState = state { stateSessions = newSessions }
    writeTVar tvar newState
    return sId

findUserIdBySessionId :: InMemory r m
                      => D.SessionId -> m (Maybe D.UserId)
findUserIdBySessionId sId = do
  tvar <- asks getter
  liftIO $ lookup sId . stateSessions <$> readTVarIO tvar
  
orThrow :: MonadError e m => Maybe a -> e -> m a
orThrow Nothing e   = throwError e
orThrow (Just a) _  = return a