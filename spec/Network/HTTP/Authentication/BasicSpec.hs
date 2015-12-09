module Network.HTTP.Authentication.BasicSpec where

import Test.Hspec
import Control.Error.Util

import Network.HTTP.Authentication.Basic

spec = do
  describe "Basic Auth Parser" $ do
    it "should parse user:pass pairs" $ do
      parseCredentials "Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==" `shouldBe` (Right $ Credentials "Aladdin" "open sesame")

    it "should error on non Base64 content" $ do
      parseCredentials "Basic foo" `shouldSatisfy` isLeft

    it "should error on non Basic auth schemes" $ do
      parseCredentials "Digest soemthing" `shouldSatisfy` isLeft

    it "should error on non : separated strings" $ do
      parseCredentials "Basic Zm9vYmFy" `shouldSatisfy` isLeft

    it "should allow empty passwords" $ do
      parseCredentials "Basic dXNlcjo=" `shouldBe` (Right $ Credentials "user" "")

    it "should allow empty usernames" $ do
      parseCredentials "Basic OnBhc3M=" `shouldBe` (Right $ Credentials "" "pass")
