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
import Credentials
import Metrics

import qualified Network.Statsd as Stats
import qualified Network.Statsd.Cluster as StatsCluster

{-
  TODO

  1. Make the cluster / connected statsd socket part of a ReaderT config
  2. Put Scotty into production mode if `SCOTTY_ENV` is absent or 'production'
-}

main :: IO ()
main = (maybe 3000 read <$> lookupEnv "PORT") >>= server

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

      case logs of
        Nothing -> return ()
        Just l -> do
          let errors = rights $ parseHerokuError <$> catMaybes (getMessage <$> l)

          let statPrefix = T.unpack app ++ ".heroku.errors"

          cluster <- liftIO metricsCluster

          liftIO $ forM_ errors $ \err ->
            let stat = statPrefix ++ "." ++ getCode err
             in StatsCluster.increment cluster stat

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
                   check <- liftIO (checkAppNameAuthentication app c)
                   bool unauthenticated (return True) check

checkAppNameAuthentication :: String -> Credentials -> IO Bool
checkAppNameAuthentication app_name auth = do
  creds <- credentialsForAppName app_name
  return $ auth == creds

parseLogs :: ActionM (Maybe [LogEntry])
parseLogs = do
  contentType <- T.unpack . fromMaybe "" <$> header "Content-Type"
  if (toLower <$> contentType) /= "application/logplex-1"
  then notAcceptable >> return Nothing
  else do
    logplexDocument <- BC.unpack <$> body

    case parseLogplex logplexDocument of
      Left _     -> unprocessable >> return Nothing
      Right logs -> return (Just logs)

unauthenticated = status unauthorized401 >> json A.Null
notAcceptable = status notAcceptable406 >> json A.Null
unprocessable = status (Status 422 "Unprocessable") >> json A.Null
created = status created201 >> json A.Null
