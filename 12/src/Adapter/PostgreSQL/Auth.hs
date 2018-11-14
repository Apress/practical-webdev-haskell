module Adapter.PostgreSQL.Auth where
  
import ClassyPrelude
import qualified Domain.Auth.Types as D
import Text.StringRandom
import Data.Has
import Data.Pool
import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple
import Data.Time

type State = Pool Connection

type PG r m = (Has State r, MonadReader r m, MonadIO m, MonadThrow m)

data Config = Config
  { configUrl :: ByteString
  , configStripeCount :: Int
  , configMaxOpenConnPerStripe :: Int
  , configIdleConnTimeout :: NominalDiffTime
  } deriving (Eq, Show)

withState :: Config -> (State -> IO a) -> IO a
withState cfg action =
  withPool cfg $ \state -> do
    migrate state
    action state

withPool :: Config -> (State -> IO a) -> IO a
withPool cfg =
  bracket initPool cleanPool
  where
    initPool = createPool openConn closeConn
                (configStripeCount cfg)
                (configIdleConnTimeout cfg)
                (configMaxOpenConnPerStripe cfg)
    cleanPool = destroyAllResources
    openConn = connectPostgreSQL (configUrl cfg)
    closeConn = close

withConn :: PG r m => (Connection -> IO a) -> m a
withConn action = do
  pool <- asks getter
  liftIO . withResource pool $ \conn -> action conn

migrate :: State -> IO ()
migrate pool = withResource pool $ \conn -> do
  result <- withTransaction conn (runMigrations False conn cmds)
  case result of
    MigrationError err -> throwString err
    _ -> return ()
  where
    cmds =  [ MigrationInitialization
            , MigrationDirectory "src/Adapter/PostgreSQL/Migrations"
            ]

addAuth :: PG r m
        => D.Auth
        -> m (Either D.RegistrationError (D.UserId, D.VerificationCode))
addAuth (D.Auth email pass) = do
  let rawEmail = D.rawEmail email
      rawPassw = D.rawPassword pass
  -- generate vCode
  vCode <- liftIO $ do
    r <- stringRandomIO "[A-Za-z0-9]{16}"
    return $ tshow rawEmail <> "_" <> r
  -- issue query
  result <- withConn $ \conn -> 
    try $ query conn qry (rawEmail, rawPassw, vCode)
  -- interpret result
  case result of
    Right [Only uId] -> return $ Right (uId, vCode)
    Right _ -> throwString "Should not happen: PG doesn't return userId"
    Left err@SqlError{sqlState = state, sqlErrorMsg = msg} ->
      if state == "23505" && "auths_email_key" `isInfixOf` msg
        then return $ Left D.RegistrationErrorEmailTaken
        else throwString $ "Unhandled PG exception: " <> show err
  where
    qry = "insert into auths \
          \(email, pass, email_verification_code, is_email_verified) \
          \values (?, crypt(?, gen_salt('bf')), ?, 'f') returning id"

setEmailAsVerified :: PG r m
                   => D.VerificationCode
                   -> m (Either D.EmailVerificationError (D.UserId, D.Email))
setEmailAsVerified vCode = do
  result <- withConn $ \conn -> query conn qry (Only vCode)
  case result of 
    [(uId, mail)] -> case D.mkEmail mail of
      Right email -> return $ Right (uId, email)
      _ -> throwString $ "Should not happen: email in DB is not valid: " <> unpack mail
    _ -> return $ Left D.EmailVerificationErrorInvalidCode
  where
    qry = "update auths \
          \set is_email_verified = 't' \
          \where email_verification_code = ? \
          \returning id, cast (email as text)"

findUserByAuth :: PG r m
               => D.Auth -> m (Maybe (D.UserId, Bool))
findUserByAuth (D.Auth email pass) = do
  let rawEmail = D.rawEmail email
      rawPassw = D.rawPassword pass
  result <- withConn $ \conn -> query conn qry (rawEmail, rawPassw)
  return $ case result of
    [(uId, isVerified)] -> Just (uId, isVerified)
    _ -> Nothing
  where
    qry = "select id, is_email_verified \
          \from auths \
          \where email = ? and pass = crypt(?, pass)"

findEmailFromUserId :: PG r m
                    => D.UserId -> m (Maybe D.Email)
findEmailFromUserId uId = do
  result <- withConn $ \conn -> query conn qry (Only uId)
  case result of
    [Only mail] -> case D.mkEmail mail of
      Right email -> return $ Just email
      _ -> throwString $ "Should not happen: email in DB is not valid: " <> unpack mail
    _ ->
      return Nothing
  where
    qry = "select cast(email as text) \
          \from auths \
          \where id = ?"