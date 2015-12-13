module Text.HerokuErrors.ParserSpec where

import Test.Hspec

import Text.HerokuErrors.Parser

spec = do
  describe "HerokuErrors Parser" $ do
    it "should parse H10 errors" $ do
      let err = parseHerokuError "at=error code=H10 desc=\"App crashed\" method=GET path=\"/\" host=myapp.herokuapp.com fwd=17.17.17.17 dyno= connect= service= status=503 bytes="
      err `shouldBe` Right (HerokuError "H10" "App crashed")

    it "should parse H14 errors" $ do
      let err = parseHerokuError "at=error code=H14 desc=\"No web processes running\" method=GET path=\"/ping\" host=ancient-savannah-2923.herokuapp.com request_id=f6e91d26-b363-4837-8208-00dcf6d7b2f0 fwd=\"120.148.233.212\" dyno= connect= service= status=503 bytes=\n"
      err `shouldBe` Right (HerokuError "H14" "No web processes running")
