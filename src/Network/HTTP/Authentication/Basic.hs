module Network.HTTP.Authentication.Basic (
  Credentials(..),
  parseCredentials,
) where

import Control.Monad

import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as B

import Text.ParserCombinators.Parsec

data Credentials = Credentials { getUsername :: String
                               , getPassword :: String
                               }

instance Show Credentials where
  show (Credentials user pass) = let concat' = mconcat [user, ":", pass]
                                     encoded = (encode . B.pack) concat'
                                  in "Basic " ++ B.unpack encoded

parseCredentials :: String -> Either ParseError Credentials
parseCredentials content = do
  decoded <- parse parseBasicCredentials "(unknown)" content

  let (user, pass) = break (/= ':') decoded

  return $ Credentials user pass

parseBasicCredentials = string "Basic" >> space >> base64String

base64Char = oneOf ("+/" ++ ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'])

base64String = do
  encoded <- many1 base64Char
  either (fail "not a base64 string") (return . B.unpack) $ decode (B.pack encoded)
