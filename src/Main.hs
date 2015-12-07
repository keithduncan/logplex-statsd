{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Web.Scotty
import Network.Wai.Middleware.RequestLogger

import Control.Monad
import Control.Monad.Trans

import Data.Text.Lazy as T
import Data.Aeson (Value (Null))
import Data.Char

import Text.Logplex.Parser
import Text.HerokuErrors.Parser
import Network.Statsd

main :: IO ()
main = (maybe 3000 read <$> lookupEnv "PORT") >>= server

server :: Int -> IO ()
server port = scotty port $ do
  middleware logStdoutDev

  post "/:app_name/logs" $ do
{-
    check authentication

    app <- param "app_name"
    contentType <- header "Content-Type"
    if (toLower <$> contentType) /= (toLower <$> "application/logplex-1")
    then error "unknown content-type status 406 not accepted"
    else error "cannot parse logplex doc"

    body <- body
    logEntries <- parseLogplex body
-}
    text $ T.pack "OK"
