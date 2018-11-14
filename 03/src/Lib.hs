module Lib
  ( someFunc
  ) where

import ClassyPrelude
import qualified Adapter.InMemory.Auth as M
import Domain.Auth

type State = TVar M.State
newtype App a = App
  { unApp :: ReaderT State IO a
  } deriving (Applicative, Functor, Monad, MonadReader State, MonadIO)

run :: State -> App a -> IO a
run state = flip runReaderT state . unApp

instance AuthRepo App where
  addAuth = M.addAuth
  setEmailAsVerified = M.setEmailAsVerified
  findUserByAuth = M.findUserByAuth
  findEmailFromUserId = M.findEmailFromUserId

instance EmailVerificationNotif App where
  notifyEmailVerification = M.notifyEmailVerification

instance SessionRepo App where
  newSession = M.newSession
  findUserIdBySessionId = M.findUserIdBySessionId

someFunc :: IO ()
someFunc = do
  state <- newTVarIO M.initialState
  run state action

action :: App ()
action = do
  let email = either undefined id $ mkEmail "ecky@test.com"
      passw = either undefined id $ mkPassword "1234ABCDefgh"
      auth = Auth email passw
  register auth
  Just vCode <- M.getNotificationsForEmail email
  verifyEmail vCode
  Right session <- login auth
  Just uId <- resolveSessionId session
  Just registeredEmail <- getUser uId
  print (session, uId, registeredEmail)
