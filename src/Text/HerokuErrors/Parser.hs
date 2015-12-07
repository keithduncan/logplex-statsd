module Text.HerokuErrors.Parser (
  parseHerokuError,

  HerokuError,
  getCode,
  getDescription,

  ParseHerokuError(..),
) where

import Control.Monad
import Control.Error.Util

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

import Text.Printf

data HerokuError = HerokuError { getCode :: String
                               , getDescription :: String
                               }

data ParseHerokuError = ParseError ParseError | NotAtError | MissingCode | MissingDesc

parseHerokuError :: String -> Either ParseHerokuError HerokuError
parseHerokuError content = do
  values <- mapLeft ParseError $ parse herokuError "(unknown)" content

  at <- note NotAtError $ lookup "at" values

  HerokuError <$>
    note MissingCode (lookup "code" values) <*>
    note MissingDesc (lookup "desc" values)

{-
  TODO this just parses H class errors, handle R and L class errors too
  <https://devcenter.heroku.com/articles/error-codes>
-}
herokuError = sepBy kvPair space

type Key = String
type Value = String
type Pair = (Key, Value)

kvPair = liftM2 (,) key (equals >> value)

key = many1 alphaNum
equals = char '='
value = try quotedValue <|> plainValue

quotedValue = quoted (many1 $ escaped '\\' "\"]")
quoted = between doubleQuote doubleQuote
doubleQuote = char '"'

escaped echar chars = let echars = echar:chars
                       in noneOf echars <|> choice (fmap (try . (char echar >>) . char) echars)

plainValue = manyTill alphaNum space
