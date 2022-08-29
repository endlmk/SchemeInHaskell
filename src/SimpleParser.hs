{-# LANGUAGE OverloadedStrings #-}

module SimpleParser
    ( parseExpression, Atom(Symbol, Number), LispVal(Atom, List)
    ) where
import Prelude (String, Double, Show, Eq, Monad (return), read, Either, Char, Integer)
-- import Text.ParserCombinators.Parsec ( letter, many, Parser )
import Text.Parsec
import Text.Parsec.Text
import Data.Either

import Data.Text (pack)

data Atom = Symbol String
          | Number Double
          deriving (Eq, Show)

data LispVal = Atom Atom
             | List [LispVal]
             deriving (Eq, Show)


symbol :: Parser LispVal
symbol = do sym <- many1 letter
            return (Atom (Symbol sym))

number :: Parser LispVal
number = do num <- many1 digit
            return (Atom (Number (read num :: Double)))

parseExpression :: String -> Either ParseError LispVal
parseExpression s = parse (symbol <|> number) "error" (pack s)

