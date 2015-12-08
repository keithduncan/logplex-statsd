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
  encoded <- parse parseBasicCredentials "(unknown)" content
  decoded <- either (fail "not a base64 string") return $ (decode . B.pack) encoded

  let (user, pass) = break (/= ':') (B.unpack decoded)

  return $ Credentials user pass

parseBasicCredentials = string "Basic" >> space >> many1 base64Char

base64Char = oneOf ("+/" ++ ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'])
