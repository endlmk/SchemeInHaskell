{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module SimpleParser
  ( parseExpression,
    LispVal (Atom, Number),
  )
where

import Data.Text (pack)
import Data.Text qualified as T
import Text.Parsec
import Text.Parsec.Text

data LispVal
  = Atom T.Text
  | Number Integer
  deriving (Eq, Show)

atom :: Parser LispVal
atom = do
  sym <- many1 letter
  return (Atom (T.pack sym))

number :: Parser LispVal
number = do
  num <- many1 digit
  return (Number (read num :: Integer))

parseExpression :: String -> Either ParseError LispVal
parseExpression s = parse (atom <|> number) "error" (pack s)
