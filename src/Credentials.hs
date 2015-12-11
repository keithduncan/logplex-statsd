module Credentials (
  credentialsForAppName,
) where

import System.Environment
import System.IO.Error

import Network.HTTP.Authentication.Basic

import Data.Char
import Data.Monoid
import Data.List (uncons)

-- Look up credentials for the given app name, fall back to the global
-- credentials if there aren't app specific ones set.
credentialsForAppName :: String -> IO Credentials
credentialsForAppName app_name = do
  let appCredentials = credentialsForName $ "API_CREDENTIALS" <> "_" <> (toUpper <$> app_name)
  catchIOError appCredentials (const defaultCredentials)

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

defaultCredentials :: IO Credentials
defaultCredentials = do
  secret <- lookupEnv "API_SECRET"

  case Credentials "" <$> secret of
    Nothing -> fail "no default credentials specified"
    Just c  -> return c
