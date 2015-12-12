module Metrics (
  metricsCluster,
  metricsClusterForConfiguration,
) where

import System.Environment
import System.IO.Error

import qualified Data.Text as T
import Data.Maybe
import Data.Either (lefts, rights)
import Data.Either.Combinators (leftToMaybe)
import Data.Monoid ((<>))
import Data.List (intercalate)

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
      -- start with the configuration and itself
      clientContexts = (\a -> (a, a)) <$> clientConfigurations
      -- map the configurations to an `IO StatsdClient`
      clientIOs = mapSnd clientFromConfiguration <$> clientContexts
      -- catch any IOError that would occur from evaluating any specific client
      clientErrors = sequence $ clientIO . mapSnd tryIOError <$> clientIOs

  clients <- (clientError <$>) <$> clientErrors

  let errors = lefts clients
  let clients' = rights clients

  if null errors
  then return $ StatsCluster.cluster clients'
  else let header = "couldn't connect to cluster, " <> show (length errors) <> " clients failed:\n"
           clientReasons = intercalate "\n" errors
           reason = header <> clientReasons
        in fail reason

  where
    mapSnd :: (b -> c) -> (a, b) -> (a, c)
    mapSnd f (x, y) = (x, f y)

    clientFromConfiguration :: String -> IO Stats.StatsdClient
    clientFromConfiguration config = maybe (fail "invalid statsd client URI") return (parseURI config) >>= Stats.fromURI

    clientIO :: (a, IO b) -> IO (a, b)
    clientIO (a, bIO) = (,) a <$> bIO

    clientError :: (String, Either IOError Stats.StatsdClient) -> Either String Stats.StatsdClient
    clientError (context, Right c) = Right c
    clientError (context, Left e)  = Left ("\tcouldn't construct client for " <> ("`" <> context <> "`") <> "\n\t" <> show e)
