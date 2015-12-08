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

{-
  1. Put statsd collector cluster config into environment
  2. Make the cluster / connected statsd socket part of a ReaderT config
-}

main :: IO ()
main = (maybe 3000 read <$> lookupEnv "PORT") >>= server

server :: Int -> IO ()
server port = scotty port $ do
  middleware logStdoutDev

  post "/:app_name/logs" $ do
    auth <- header "Authorization"

{-
    1. get app name
    2. check per-app authentication
    3. check content-type
    4. parse body as application/logplex-1
    5. map log entry messages
    6. parse messages as heroku errors
    7. map each error to a statsd client increment action

    app <- param "app_name"
    contentType <- header "Content-Type"
    if (toLower <$> contentType) /= (toLower <$> "application/logplex-1")
    then error "unknown content-type status 406 not accepted"
    else error "cannot parse logplex doc"

    body <- body
    logEntries <- parseLogplex body
-}
    text $ T.pack "OK"
