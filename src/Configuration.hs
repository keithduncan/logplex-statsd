{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Configuration (
  Environment(..),
  getEnvironment,
  Config(..),
  getConfig,

  ConfigM(..),

  Configuration.getPort,

  getSettings,
  getOptions,
) where

import qualified System.Environment as Env

import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.IO.Class (MonadIO)

import Data.Default (def)

import Web.Scotty.Trans (Options, settings, verbose)
import Network.Wai.Handler.Warp
import Metrics
import qualified Network.Statsd.Cluster as Statsd

data Environment = Development | Production deriving (Show)

instance Read Environment where
  readsPrec _ e = case e of
                    "development" -> [(Development, "")]
                    "production" -> [(Production, "")]

getEnvironment :: IO Environment
getEnvironment = maybe Production read <$> Env.lookupEnv "SCOTTY_ENV"

getPort :: IO Int
getPort = maybe 3000 read <$> Env.lookupEnv "PORT"

getOptions :: Environment -> IO Options
getOptions e = do
  s <- getSettings e
  return def {
    settings = s,
    verbose = case e of
      Development -> 1
      _           -> 0
  }

getSettings :: Environment -> IO Settings
getSettings e = do
  port <- Configuration.getPort
  return $ setPort port defaultSettings

data Config = Config { environment :: Environment
                     , metrics :: Statsd.Cluster
                     }

newtype ConfigM a = ConfigM { runConfigM :: ReaderT Config IO a
                            } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

getConfig :: IO Config
getConfig = Config <$>
  getEnvironment <*>
  metricsCluster
