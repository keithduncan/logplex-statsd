{-# LANGUAGE FlexibleContexts #-}

module Text.HerokuErrors.Parser (
  parseHerokuError,

  HerokuError(..),
) where

import Control.Monad
import Control.Error.Util

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
    check "Not an at=error log entry" (== "error") =<< note "Missing at key" (lookup "at" values)

    HerokuError <$>
      (check "Not an H category error" ((== 'H') . head) =<< note "Missing code key" (lookup "code" values)) <*>
      note "Missing desc key" (lookup "desc" values)
  where
    check :: a -> (b -> Bool) -> b -> Either a b
    check x f y = if f y
                  then Right y
                  else Left x

kvPair = liftM2 (,) key (equals >> value)

key = P.many1 P.alphaNum
equals = P.char '='
value = P.try quotedValue P.<|> plainValue

quotedValue = quoted (P.many1 $ escaped '\\' "\"]")
quoted = P.between doubleQuote doubleQuote
doubleQuote = P.char '"'

escaped echar chars = let echars = echar:chars
                       in P.noneOf echars P.<|> P.choice (fmap (P.try . (P.char echar >>) . P.char) echars)

plainValue = P.many1 P.alphaNum

-- rError

rError = genericError 'R'

-- lError

lError = genericError 'L'

-- Generic

genericError category = HerokuError <$>
  (P.string "Error" >> P.space >> errorCode category) <*>
  (P.space >> P.many1 P.anyChar)

errorCode category = liftM2 (:) (P.char category) (P.many1 P.digit)
