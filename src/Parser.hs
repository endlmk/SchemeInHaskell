{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parseExpression,
    LispVal (Atom, Number, String, Bool),
  )
where

import Data.Text (pack)
import Data.Text qualified as T
import Text.Parsec
import Text.Parsec.Text

data LispVal
  = Atom T.Text
  | Number Integer
  | String T.Text
  | Bool Bool
  deriving (Eq, Show)

atom :: Parser LispVal
atom = do
  sym <- many1 letter
  return (Atom (T.pack sym))

number :: Parser LispVal
number = do
  num <- many1 digit
  return (Number (read num :: Integer))

negNumber :: Parser LispVal
negNumber = do
  _ <- char '-'
  num <- many1 digit
  return (Number ((read num :: Integer) * (-1)))

parseString :: Parser LispVal
parseString = do
  _ <- char '\"'
  str <- many (noneOf "\"")
  _ <- char '\"'
  return (String (T.pack str))

parseBool :: Parser LispVal
parseBool = do
  _ <- char '#'
  c <- oneOf "tf"
  return (Bool (c == 't'))

parseExpression :: String -> Either ParseError LispVal
parseExpression s =
  parse
    ( atom
        <|> number
        <|> negNumber
        <|> parseString
        <|> parseBool
    )
    "error"
    (pack s)
