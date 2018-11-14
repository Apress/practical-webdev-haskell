{-# OPTIONS_GHC -fno-warn-orphans #-}

module Adapter.HTTP.API.Types.Auth where

import ClassyPrelude
import Domain.Auth.Types
import Data.Aeson
import Adapter.HTTP.API.Types.AesonHelper

instance FromJSON Email where
  parseJSON =
    withText "Email" $ withSmartConstructor mkEmail

instance FromJSON Password where
  parseJSON =
    withText "Password" $ withSmartConstructor mkPassword

$(map concat . sequence $
    [ deriveJSONRecord ''Auth
    , deriveToJSONUnwrap ''Email
    , deriveToJSONUnwrap ''Password
    , deriveJSONSumType ''RegistrationError
    , deriveJSONSumType ''EmailVerificationError
    , deriveJSONSumType ''LoginError
    ])