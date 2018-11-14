module Domain.Auth.Types ( 
  -- * Types
  Auth(..),
  Email(rawEmail),
  mkEmail,
  Password(rawPassword),
  mkPassword,
  UserId,
  VerificationCode,
  SessionId,
  RegistrationError(..),
  EmailVerificationError(..),
  LoginError(..),

  -- * Services
  AuthService(..)
) where

import ClassyPrelude
import Domain.Validation
import Text.Regex.PCRE.Heavy

newtype Email = Email { rawEmail :: Text } deriving (Show, Eq, Ord)

mkEmail :: Text -> Either [ErrMsg] Email
mkEmail =
  validate Email
    [ regexMatches
        [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|]
        "Not a valid email"
    ]

newtype Password = Password { rawPassword :: Text } deriving (Show, Eq)

mkPassword :: Text -> Either [ErrMsg] Password
mkPassword =
  validate Password
    [ lengthBetween 5 50 "Should between 5 and 50"
    , regexMatches [re|\d|] "Should contain number"
    , regexMatches [re|[A-Z]|] "Should contain uppercase letter"
    , regexMatches [re|[a-z]|] "Should contain lowercase letter"
    ]

data Auth = Auth
  { authEmail :: Email
  , authPassword :: Password
  } deriving (Show, Eq)

type UserId = Int

type VerificationCode = Text

type SessionId = Text

data RegistrationError
  = RegistrationErrorEmailTaken
  deriving (Show, Eq)

data EmailVerificationError
  = EmailVerificationErrorInvalidCode
  deriving (Show, Eq)

data LoginError
  = LoginErrorInvalidAuth
  | LoginErrorEmailNotVerified
  deriving (Show, Eq)


class (Monad m) => AuthService m where
  register :: Auth -> m (Either RegistrationError ())
  verifyEmail :: VerificationCode -> m (Either EmailVerificationError ())
  login :: Auth -> m (Either LoginError SessionId)
  resolveSessionId :: SessionId -> m (Maybe UserId)
  getUser :: UserId -> m (Maybe Email)
