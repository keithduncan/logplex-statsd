{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Web.Scotty
import Network.Wai.Middleware.RequestLogger

import Data.Text as T

import Text.Logplex.Parser
import Network.Statsd

main :: IO ()
main = (maybe 3000 read <$> lookupEnv "PORT") >>= server

server :: Int -> IO ()
server port = scotty port $ do
  middleware logStdoutDev

  get "/" $ do
    greeting <- param "greeting"
    html $ mconcat ["Hello, ", greeting, "!"]

{-
server :: Int -> IO ()
server port = scotty port $ do
  post "/:app_name/logs" $ do
    app <- param "app_name" :: T.Text

    sendMetrics <- getWhitelistedApp app

    if sendMetrics
    then error "cannot send metrics"
    else error "cannot render 404"
-}

{- This may be updated to check a static list, consult a file, environment
   variable, or another web service -}
getWhitelistedApp :: String -> IO Bool
getWhitelistedApp = const $ return True
