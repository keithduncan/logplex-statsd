{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Configuration (
  Environment(..),
  getEnvironment,
  Config(..),
  getConfig,

  ConfigM(..),
) where

import qualified System.Environment as Env

import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.IO.Class (MonadIO)

import Metrics
import qualified Network.Statsd.Cluster as Statsd

data Environment = Development | Production deriving (Read)

getEnvironment :: IO Environment
getEnvironment = maybe Production read <$> Env.lookupEnv "SCOTTY_ENV"

getPort :: IO Int
getPort = maybe 3000 read <$> Env.lookupEnv "PORT"

data Config = Config { environment :: Environment
                     , metrics :: Statsd.Cluster
                     }

newtype ConfigM a = ConfigM { runConfigM :: ReaderT Config IO a
                            } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

getConfig :: IO Config
getConfig = Config <$>
  getEnvironment <*>
  metricsCluster
