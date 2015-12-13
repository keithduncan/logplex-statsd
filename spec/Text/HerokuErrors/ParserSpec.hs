module Text.HerokuErrors.ParserSpec where

import Test.Hspec

import Text.HerokuErrors.Parser

spec = do
  describe "HerokuErrors Parser" $ do
    it "should parse H10 errors" $ do
      let err = parseHerokuError "at=error code=H10 desc=\"App crashed\" method=GET path=\"/\" host=myapp.herokuapp.com fwd=17.17.17.17 dyno= connect= service= status=503 bytes="
      err `shouldBe` Right (HerokuError "H10" "App crashed")
