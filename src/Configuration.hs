module Configuration (
  Environment(..),
  getEnvironment,
  Config(..),
  getConfig,
) where

import qualified System.Environment as Env

import Metrics
import qualified Network.Statsd.Cluster as Statsd

data Environment = Development | Production deriving (Read)

getEnvironment :: IO Environment
getEnvironment = maybe Production read <$> Env.lookupEnv "SCOTTY_ENV"

data Config = Config { environment :: Environment
                     , metrics :: Statsd.Cluster
                     }

getConfig :: IO Config
getConfig = Config <$>
  getEnvironment <*>
  metricsCluster
