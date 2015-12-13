{-# LANGUAGE FlexibleContexts #-}

module Text.HerokuErrors.Parser (
  parseHerokuError,

  HerokuError(..),
) where

import Control.Monad
import Control.Error.Util

import Data.Maybe

import qualified Text.ParserCombinators.Parsec as P

data HerokuError = HerokuError { getCode :: String
                               , getDescription :: String
                               } deriving (Show, Eq)

parseHerokuError :: String -> Either P.ParseError HerokuError
parseHerokuError = P.parse herokuError "(unknown)"

herokuError = P.choice (P.try <$> [ hError, rError, lError ]) <* P.eof

-- hError

hError = do
  values <- P.sepBy kvPair P.space

  either fail return $ do
    let atValue = note "log line should have an `at` key" (lookup "at" values)
    at <- check "`at` key should be `error`" (== "error") =<< atValue

    HerokuError <$>
      (check "`code` key should start with 'H'" ((== 'H') . head) =<< note "log line should have a `code` key" (lookup "code" values)) <*>
      note "log line should have a `desc` key" (lookup "desc" values)
  where
    check :: a -> (b -> Bool) -> b -> Either a b
    check x f y = if f y
                  then Right y
                  else Left x

kvPair = liftM2 (,) key (equals >> value)

key = P.many1 (P.oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_")
equals = P.char '='
value = P.choice $ P.try <$> [ quotedValue, plainValue ]

quotedValue = quoted (P.many1 $ escaped '\\' "\"]")
quoted = P.between doubleQuote doubleQuote
doubleQuote = P.char '"'

escaped echar chars = let echars = echar:chars
                       in P.noneOf echars P.<|> P.choice (fmap (P.try . (P.char echar >>) . P.char) echars)

plainValue = P.many (P.noneOf " ")

-- rError

rError = genericError 'R'

-- lError

lError = genericError 'L'

-- Generic

genericError category = HerokuError <$>
  (P.string "Error" >> P.space >> errorCode category) <*>
  (fromMaybe "" <$> P.optionMaybe (P.space >> P.many1 P.anyChar))

errorCode category = liftM2 (:) (P.char category) (P.many1 P.digit)
