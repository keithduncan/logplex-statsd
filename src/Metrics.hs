module Metrics (
  metricsCluster,
  metricsClusterForConfiguration,
) where

import System.Environment

import qualified Data.Text as T
import Data.Maybe

import Network.URI

import qualified Network.Statsd as Stats
import qualified Network.Statsd.Cluster as StatsCluster

metricsCluster :: IO StatsCluster.Cluster
metricsCluster = do
  clusterConfig <- lookupEnv "METRICS_CLUSTER"

  case clusterConfig of
    Nothing -> fail "missing cluster config"
    Just c  -> metricsClusterForConfiguration c

metricsClusterForConfiguration :: String -> IO StatsCluster.Cluster
metricsClusterForConfiguration config = do
  let clientConfigurations = T.unpack <$> T.split (==',') (T.pack config)
      uris = catMaybes $ parseURI <$> clientConfigurations
      clients = sequence (Stats.fromURI <$> uris)

  clients' <- clients

  if length clients' /= length clientConfigurations
  then fail "couldn't decode cluster config"
  else StatsCluster.cluster <$> clients
