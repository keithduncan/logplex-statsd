{-# LANGUAGE OverloadedStrings #-}

import System.Environment

import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Network.HTTP.Types
import Network.HTTP.Authentication.Basic

import Control.Monad
import Control.Monad.Trans

import qualified Data.Text.Lazy as T
import Data.Aeson (Value (Null))
import Data.Char
import Data.Bool
import Data.Maybe
import Data.Either
import Data.Either.Combinators (fromRight)
import qualified Data.ByteString.Lazy.Char8 as BC

import Text.Logplex.Parser
import Text.Syslog.Parser
import Text.HerokuErrors.Parser
import Network.Statsd.Cluster

{-
  1. Put statsd collector cluster config into environment
  2. Make the cluster / connected statsd socket part of a ReaderT config
-}

main :: IO ()
main = (maybe 3000 read <$> lookupEnv "PORT") >>= server

metricsCluster :: Cluster
metricsCluster = error "no cluster config"

server :: Int -> IO ()
server port = scotty port $ do
  middleware logStdoutDev

  post "/:app_name/logs" $ do
    {-
        1. check per-app authentication -> 401 unauthorized
        2. check content-type -> 406 not acceptable
        3. parse body as application/logplex-1 -> 422 unprocessable
        4. map log entry messages
        5. parse messages as heroku errors
        6. map each error to a statsd client increment action
    -}

    checkAuthentication

    app <- param "app_name" :: ActionM T.Text
    logs <- parseLogs

    let errors = rights $ parseHerokuError <$> catMaybes (getMessage <$> logs)

    let statPrefix = T.unpack app ++ "heroku.errors"

    liftIO $ forM_ errors $ \err ->
      let stat = statPrefix ++ "." ++ getCode err
       in increment metricsCluster stat

    created

checkAuthentication :: ActionM ()
checkAuthentication = do
  app <- param "app_name"
  auth <- maybe "" T.unpack <$> header "Authorization" >>= either (fail "couldn't parse Authorization header") return . parseCredentials

  check <- liftIO (checkAppAuthentication app auth)

  bool unauthenticated (return ()) check

-- TODO make this variable based on the app name, single global authentication
-- credentials are bad practice.
checkAppAuthentication :: String -> Credentials -> IO Bool
checkAppAuthentication app auth = return True

parseLogs :: ActionM [LogEntry]
parseLogs = do
  contentType <- T.unpack . fromMaybe "" <$> header "Content-Type"
  bool notAcceptable (return ()) ((toLower <$> contentType) == "application/logplex-1")

  logplexDocument <- BC.unpack <$> body

  let parse = parseLogplex logplexDocument
  unless (isRight parse) unprocessable

  return $ fromRight [] parse

unauthenticated = status unauthorized401 >> json Null
notAcceptable = status notAcceptable406 >> json Null
unprocessable = status (Status 422 "Unprocessable") >> json Null
created = status created201 >> json Null
