module Domain.Auth.TypesSpec (spec) where

import ClassyPrelude
import Test.Hspec
import Domain.Auth.Types

spec :: Spec
spec = do
  describe "mkEmail" $ do
    describe "should pass" $ 
      mkEmailSpec "ecky@test.com" True
    describe "should fail" $ do
      mkEmailSpec "invalid email@test.com" False
      mkEmailSpec "email@test." False
      mkEmailSpec "test.com" False

  describe "mkPassword" $ do
    describe "should pass" $
      mkPasswordSpec "abcDEF123" []
    describe "should fail" $ do
      mkPasswordSpec "aA1" ["Should between 5 and 50"]
      mkPasswordSpec (fromString . take 51 . join $ repeat "aA1") ["Should between 5 and 50"]
      mkPasswordSpec "abcDEF" ["Should contain number"]
      mkPasswordSpec "abc123" ["Should contain uppercase letter"]
      mkPasswordSpec "ABC123" ["Should contain lowercase letter"]

mkEmailSpec :: Text -> Bool -> Spec
mkEmailSpec email isValid =
  it (unpack email) $
    case (isValid, mkEmail email) of
      (True, result) ->
        result `shouldSatisfy` either (const False) ((email ==) . rawEmail)
      (False, result) ->
        result `shouldSatisfy` either (["Not a valid email"] ==) (const False)

mkPasswordSpec :: Text -> [Text] -> Spec
mkPasswordSpec password errMsgs =
  it (unpack password) $
    case (errMsgs, mkPassword password) of
      ([], result) ->
        result `shouldSatisfy` either (const False) ((password ==) . rawPassword)
      (msgs, result) ->
        result `shouldSatisfy` either (msgs ==) (const False)