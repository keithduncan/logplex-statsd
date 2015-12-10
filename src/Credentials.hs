module Credentials (
  credentialsForAppName,
) where

import System.Environment

import Network.HTTP.Authentication.Basic

import Data.Char
import Data.Monoid
import Data.List (uncons)

credentialsPrefix = "API_CREDENTIALS"

-- Look up credentials for the given app name, fall back to the global
-- credentials if there aren't app specific ones set.
credentialsForAppName :: String -> IO Credentials
credentialsForAppName app_name = do
  let defaultCredentials = credentialsForName credentialsPrefix
  let appCredentials = credentialsForName $ credentialsPrefix <> "_" <> (toUpper <$> app_name)

  appCredentials

credentialsForName :: String -> IO Credentials
credentialsForName name = do
  credConfig <- lookupEnv name

  case breakOn ':' <$> credConfig of
    Nothing           -> fail ("no credentials for " <> name)
    Just (user, pass) -> return $ Credentials user pass

  where
    breakOn :: Eq a => a -> [a] -> ([a], [a])
    breakOn x xs = let p = (/= x)
                       head' = takeWhile p xs
                       tail' = dropWhile p xs
                       tail'' = case uncons tail' of
                                  Nothing     -> tail'
                                  Just (_, y) -> y
                    in (head', tail'')
