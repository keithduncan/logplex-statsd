{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import System.Time
import System.IO.Error

import Web.Scotty.Trans
import Network.Wai.Middleware.RequestLogger
import Network.HTTP.Types
import Network.HTTP.Authentication.Basic

import Control.Monad
import Control.Monad.Trans
import Control.Error.Util
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)

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
import Configuration

import qualified Network.Statsd as Stats
import qualified Network.Statsd.Cluster as StatsCluster

main :: IO ()
main = getConfig >>= runApplication

runApplication :: Config -> IO ()
runApplication c = do
  o <- getOptions (environment c)
  let run m = runReaderT (runConfigM m) c
  scottyOptsT o run (application (environment c))

type Action a = ActionT T.Text ConfigM a

defaultH :: Environment -> T.Text -> Action ()
defaultH e x = json $ case e of
                        Development -> A.object ["error" A..= showError x]
                        _           -> A.Null

application :: Environment -> ScottyT T.Text ConfigM ()
application e = do
  middleware $ case e of
    Development -> logStdoutDev
    _           -> logStdout

  defaultHandler (defaultH e)

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
      app <- param "app_name" :: Action T.Text
      logs <- parseLogs

      case logs of
        Nothing -> return ()
        Just l -> do
          let errors = rights $ parseHerokuError <$> catMaybes (getMessage <$> l)

          let statPrefix = T.unpack app ++ ".heroku.errors"

          cluster <- lift (asks metrics)

          liftIO $ forM_ errors $ \err ->
            let stat = statPrefix ++ "." ++ getCode err
             in StatsCluster.increment cluster stat

          created

  notFound (status status404 >> json A.Null)

-- TODO make this 'throw' an error which is handled in the application to mean
-- unauthenticated
checkAuthentication :: Action Bool
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
checkAppNameAuthentication app_name auth = catchIOError ((== auth) <$> credentialsForAppName app_name) (const $ return False)

parseLogs :: Action (Maybe [LogEntry])
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
