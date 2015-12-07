module Text.HerokuErrors.Parser (
  parseHerokuError,

  HerokuError,
  getCode,
  getDescription,
) where

import Control.Monad

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

import Text.Printf

data HerokuError = HerokuError { getCode :: String
                               , getDescription :: String
                               }

parseHerokuError :: String -> Either String HerokuError
parseHerokuError content = do
  values <- mapLeft (\err -> head $ messageString <$> errorMessages err) $ parse herokuError "(unknown)" content

  at <- hlookup "at" values

  HerokuError <$>
    hlookup "code" values <*>
    hlookup "desc" values

hlookup :: String -> [(String, a)] -> Either String a
hlookup k vs = maybe (Left $ printf "missing `$s` key" k) Right (lookup k vs)

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right x) = Right x

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
