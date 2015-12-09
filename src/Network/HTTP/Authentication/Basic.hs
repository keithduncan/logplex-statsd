module Network.HTTP.Authentication.Basic (
  Credentials(..),
  parseCredentials,
  encode,
) where

import Control.Monad.Except()

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B

import Text.ParserCombinators.Parsec

data Credentials = Credentials { getUsername :: String
                               , getPassword :: String
                               } deriving (Show, Eq)

encode :: Credentials -> String
encode c = let concat' = mconcat [getUsername c, ":", getPassword c]
               encoded = (B.unpack . B64.encode . B.pack) concat'
            in "Basic " ++ encoded

parseCredentials :: String -> Either ParseError Credentials
parseCredentials = parse parseBasicCredentials "(unknown)"

parseBasicCredentials = do
  decoded <- string "Basic" >> space >> base64String

  let pair = break (== ':') decoded

  case pair of
    (user, ':':pass) -> return $ Credentials user pass
    _                -> fail "not a username password pair"

base64Char = oneOf ("+/=" ++ ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'])

base64String = do
  encoded <- many1 base64Char
  either fail (return . B.unpack) $ B64.decode (B.pack encoded)
