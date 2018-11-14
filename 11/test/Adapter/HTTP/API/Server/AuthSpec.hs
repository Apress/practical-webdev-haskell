module Adapter.HTTP.API.Server.AuthSpec where

import ClassyPrelude
import Domain.Auth.Types
import Adapter.HTTP.Fixture
import Test.Hspec hiding (pending)
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types

spec :: Spec
spec = do
  describe "POST /api/auth/register" $ do
    with (app emptyFixture) $ do
      it "should reject malformed JSON" $
        post "/api/auth/register" ""
          `shouldRespondWith`
            [json|
              {"email":["Not a valid email"]
              ,"password":["Should between 5 and 50"
                          ,"Should contain number"
                          ,"Should contain uppercase letter"
                          ,"Should contain lowercase letter"
                          ]
              }
            |] { matchStatus = 400 }
      it "should reject invalid input" $
        post "/api/auth/register" "{}"
          `shouldRespondWith`
            [json|
              {"email":["Not a valid email"]
              ,"password":["Should between 5 and 50"
                          ,"Should contain number"
                          ,"Should contain uppercase letter"
                          ,"Should contain lowercase letter"
                          ]
              }
            |] { matchStatus = 400 }
    
    let emailTakenFixture = emptyFixture 
          { _register = \_ -> return $ Left RegistrationErrorEmailTaken }
    with (app emailTakenFixture) $
      it "should reject account creation" $
        post "/api/auth/register"
             [json|{"email": "abc@test.com", "password":"abcDEF123"}|]
          `shouldRespondWith` [json|"EmailTaken"|] { matchStatus = 400 }
    
    let successFixture = emptyFixture 
          { _register = \_ -> return $ Right () }
    with (app successFixture) $
      it "successfully build account creation" $
        post "/api/auth/register"
             [json|{"email": "abc@test.com", "password":"abcDEF123"}|]
          `shouldRespondWith` "" { matchStatus = 200 }



  describe "POST /api/auth/verifyEmail" $ do
    let verifyEmailErrFixture = emptyFixture
          { _verifyEmail = \_ -> return $ Left EmailVerificationErrorInvalidCode }
    with (app verifyEmailErrFixture) $
      it "should handle email verification failure" $
        post "/api/auth/verifyEmail" [json|"abc"|]
          `shouldRespondWith` [json|"InvalidCode"|] { matchStatus = 400 }
          
    let verifyEmailSuccessFixture = emptyFixture
          { _verifyEmail = \vCode -> case vCode of
              "abc" -> return $ Right ()
              _ -> error "Should not be called"
          }
    with (app verifyEmailSuccessFixture) $
      it "should handle email verification failure" $
        post "/api/auth/verifyEmail" [json|"abc"|]
          `shouldRespondWith` "" { matchStatus = 200 }



  describe "POST /api/auth/login" $ do
    with (app emptyFixture) $ do
      it "should reject malformed JSON" $
        post "/api/auth/login" ""
          `shouldRespondWith`
            [json|
              {"email":["Not a valid email"]
              ,"password":["Should between 5 and 50"
                          ,"Should contain number"
                          ,"Should contain uppercase letter"
                          ,"Should contain lowercase letter"
                          ]
              }
            |] { matchStatus = 400 }
      it "should reject invalid input" $
        post "/api/auth/login" "{}"
          `shouldRespondWith`
            [json|
              {"email":["Not a valid email"]
              ,"password":["Should between 5 and 50"
                          ,"Should contain number"
                          ,"Should contain uppercase letter"
                          ,"Should contain lowercase letter"
                          ]
              }
            |] { matchStatus = 400 }

    let invalidAuthFixture = emptyFixture
          { _login = \_ -> return $ Left LoginErrorInvalidAuth }
    with (app invalidAuthFixture) $
      it "should reject invalid auth" $
        post "/api/auth/login"
             [json|{"email": "abc@test.com", "password":"abcDEF123"}|]
          `shouldRespondWith` [json|"InvalidAuth"|] { matchStatus = 400 }

    let unverifiedEmailFixture = emptyFixture
          { _login = \_ -> return $ Left LoginErrorEmailNotVerified }
    with (app unverifiedEmailFixture) $
      it "should reject unverified email" $
        post "/api/auth/login"
             [json|{"email": "abc@test.com", "password":"abcDEF123"}|]
          `shouldRespondWith` [json|"EmailNotVerified"|] { matchStatus = 400 }

    let successfulLoginFixture = emptyFixture
          { _login = \_ -> return $ Right "sId" }
    with (app successfulLoginFixture) $
      it "should return cookie when logged in successfully" $
        post "/api/auth/login"
             [json|{"email": "abc@test.com", "password":"abcDEF123"}|]
          `shouldRespondWith` 200 { matchHeaders = [hasHeader "Set-Cookie"] }



  describe "GET /api/users" $ do
    with (app emptyFixture) $
      it "should reject non-cookie user" $
        get "/api/users"
          `shouldRespondWith` [json|"AuthRequired"|] { matchStatus = 401 }
    
    let invalidSessionFixture = emptyFixture
          { _resolveSessionId = \_ -> return Nothing }
    with (app invalidSessionFixture) $
      it "should return user info successfully" $
        request "GET" "/api/users" [("Cookie", "sId=sId")] ""
          `shouldRespondWith` [json|"AuthRequired"|] { matchStatus = 401 }
    
    let email = either (error . show) id $ mkEmail "abc@def.com"

    let userNotFoundFixture = emptyFixture
          { _resolveSessionId = \sId -> case sId of
              "sId" -> return $ Just 1
              _ -> return Nothing
          , _getUser = const $ return Nothing
          }
    with (app userNotFoundFixture) $
      it "should throw error if user not found but session found" $
        request "GET" "/api/users" [("Cookie", "sId=sId")] ""
          `shouldRespondWith` 500

    let userFoundFixture = userNotFoundFixture
          { _getUser = \uId -> case uId of
              1 -> return $ Just email
              _ -> return Nothing
          }
    with (app userFoundFixture) $
      it "should return user info successfully" $
        request "GET" "/api/users" [("Cookie", "sId=sId")] ""
          `shouldRespondWith` [json|"abc@def.com"|] { matchStatus = 200 }

hasHeader :: HeaderName -> MatchHeader
hasHeader name = 
  MatchHeader $ \headers _ ->
    case find (\(n, _) -> n == name) headers of
      Nothing -> Just $ "Header not found: " <> show name
      Just _ -> Nothing
    
