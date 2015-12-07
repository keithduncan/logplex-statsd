module Text.HerokuErrors.Parser (
  parseHerokuError,

  HerokuError,
  getCode,
  getDescription,

  ParseError(..),
) where

import Control.Monad
import Control.Error.Util

import qualified Text.ParserCombinators.Parsec as P

import Text.Printf

data HerokuError = HerokuError { getCode :: String
                               , getDescription :: String
                               }

data ParseError = ParseError P.ParseError | NotAtError | MissingCode | MissingDesc

parseHerokuError :: String -> Either ParseError HerokuError
parseHerokuError content = do
  values <- either (Left . ParseError) Right $ P.parse herokuError "(unknown)" content

  at <- note NotAtError $ lookup "at" values

  HerokuError <$>
    note MissingCode (lookup "code" values) <*>
    note MissingDesc (lookup "desc" values)

{-
  TODO this just parses H class errors, handle R and L class errors too
  <https://devcenter.heroku.com/articles/error-codes>
-}
herokuError = P.choice $ P.try <$> [ hError, rError ]

-- hError

hError = P.sepBy kvPair P.space <* P.eof

type Key = String
type Value = String
type Pair = (Key, Value)

kvPair = liftM2 (,) key (equals >> value)

key = P.many1 P.alphaNum
equals = P.char '='
value = P.try quotedValue P.<|> plainValue

quotedValue = quoted (P.many1 $ escaped '\\' "\"]")
quoted = P.between doubleQuote doubleQuote
doubleQuote = P.char '"'

escaped echar chars = let echars = echar:chars
                       in P.noneOf echars P.<|> P.choice (fmap (P.try . (P.char echar >>) . P.char) echars)

plainValue = P.manyTill P.alphaNum P.space

-- rError

rError = do
  code <- P.string "Error" >> P.space >> rVal
  desc <- P.space >> P.between (P.char '(') (P.char ')') (P.many1 $ P.noneOf ")")
  return [("at", "error"), ("code", code), ("desc", desc)]

rVal = liftM2 (:) (P.char 'R') (P.many1 P.digit)
