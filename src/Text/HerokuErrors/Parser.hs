module Text.HerokuErrors.Parser (
  parseHerokuError,

  HerokuError,
  getCode,
  getDescription,
) where

import Control.Monad

import Text.ParserCombinators.Parsec

data HerokuError = HerokuError { getCode :: String
                               , getDescription :: String
                               }

parseHerokuError :: String -> Maybe HerokuError
parseHerokuError content = do
  values <- either (const Nothing) Just $ parse herokuError "(unknown)" content

  at <- lookup "at" values

  HerokuError <$>
    lookup "code" values <*>
    lookup "desc" values

{-
  at <- lookup "at" values
  case at of
    Nothing  -> fail "couldn't parse heroku log entry as error"
    Just err -> HerokuError <$>
      lookup "code" values <*>
      lookup "desc" values
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
