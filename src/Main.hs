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
    checkAuth

{-
    1. check per-app authentication -> 401 unauthorized
    2. check content-type -> 406 not acceptable
    3. parse body as application/logplex-1 -> 422 unprocessable
    4. map log entry messages
    5. parse messages as heroku errors
    6. map each error to a statsd client increment action

    contentType <- header "Content-Type"
    if (toLower <$> contentType) /= (toLower <$> "application/logplex-1")
    then error "unknown content-type status 406 not accepted"
    else error "cannot parse logplex doc"

    body <- body
    logEntries <- parseLogplex body
-}
    text $ T.pack "OK"
