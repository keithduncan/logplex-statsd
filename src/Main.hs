{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import System.Time

import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Network.HTTP.Types
import Network.HTTP.Authentication.Basic

import Control.Monad
import Control.Monad.Trans
import Control.Error.Util

import qualified Data.Text.Lazy as T
import qualified Data.Aeson as A
import Data.Char
import Data.Maybe
import Data.Either (rights)
import Data.Either.Combinators (fromRight, fromRight')
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as BC

import Text.Logplex.Parser
import Text.Syslog.Parser
import Text.HerokuErrors.Parser
import Network.Statsd.Cluster

{-
  TODO

  1. Put statsd collector cluster config into environment
  2. Make the cluster / connected statsd socket part of a ReaderT config
  3. Implement environment based authentication in checkAppAuthentication
-}

main :: IO ()
main = (maybe 3000 read <$> lookupEnv "PORT") >>= server

metricsCluster :: Cluster
metricsCluster = error "no cluster config"

server :: Int -> IO ()
server port = scotty port $ do
  middleware logStdoutDev

  get "/_ping" $ do
    (TOD sec _) <- liftIO getClockTime
    let ping = M.fromList [("now", show sec), ("status", "ok")] :: M.Map String String

    status ok200
    json ping

  post "/:app_name/logs" $ do
    {-
        1. check per-app authentication -> 401 unauthorized
        2. check content-type -> 406 not acceptable
        3. parse body as application/logplex-1 -> 422 unprocessable
        4. map log entry messages
        5. parse messages as heroku errors
        6. map each error to a statsd client increment action
    -}

    auth <- checkAuthentication
    when auth $ do
      app <- param "app_name" :: ActionM T.Text
      logs <- parseLogs

      unless (null logs) $ do
        let errors = rights $ parseHerokuError <$> catMaybes (getMessage <$> logs)

        let statPrefix = T.unpack app ++ "heroku.errors"

        liftIO $ forM_ errors $ \err ->
          let stat = statPrefix ++ "." ++ getCode err
           in increment metricsCluster stat

        created

-- TODO make this 'throw' an error which is handled in the application to mean
-- unauthenticated
checkAuthentication :: ActionM Bool
checkAuthentication = do
  app <- param "app_name"

  let unauthenticated = Main.unauthenticated >> return False

  auth <- header "Authorization" >>= \h -> return $ T.unpack <$> h
  case auth of
    Nothing -> unauthenticated
    Just a  -> let credentials = parseCredentials a
               in case credentials of
                 Left er -> unauthenticated
                 Right c -> do
                   check <- liftIO (checkAppAuthentication app c)
                   bool unauthenticated (return True) check

-- TODO make this variable based on the app name, single global authentication
-- credentials are bad practice.
checkAppAuthentication :: String -> Credentials -> IO Bool
checkAppAuthentication app auth = return True

parseLogs :: ActionM [LogEntry]
parseLogs = do
  contentType <- T.unpack . fromMaybe "" <$> header "Content-Type"
  if (toLower <$> contentType) /= "application/logplex-1"
  then notAcceptable >> return []
  else do
    logplexDocument <- BC.unpack <$> body

    case parseLogplex logplexDocument of
      Left _     -> unprocessable >> return []
      Right logs -> return logs

unauthenticated = status unauthorized401 >> json A.Null
notAcceptable = status notAcceptable406 >> json A.Null
unprocessable = status (Status 422 "Unprocessable") >> json A.Null
created = status created201 >> json A.Null
