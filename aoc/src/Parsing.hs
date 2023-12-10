module Parsing
    ( testParsingLib,
      Parser(P),
      untilDigit,
      lastDigit,
      parse,
      item,
      digit
    ) where


import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])
--data SatParser a = SP (Parser a)

parse :: Parser a -> String -> [(a, String)]
parse (P pf) inp = pf inp

instance Functor Parser where
-- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (P a) = P (\input -> case a input of
                                [] -> []
                                [(c,str)] -> [(f c, str)])

instance Applicative Parser where
  pure c = P (\inp -> [(c, inp)])

-- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pab <*> pa = P (\inp -> case parse pab inp of
                     [] -> []
                     [(f,str)] -> parse (fmap f pa) str)

instance Monad Parser where
  return = pure

-- >>= :: Parser a -> (a -> Parser b) -> Parser b
  pa >>= f = P (\inp -> case parse pa inp of
                   [] -> []
                   [(c,str)] -> parse (f c) str)

instance Alternative Parser where
  empty = P(\inp -> [])

  p <|> q = P (\inp -> case parse p inp of
                  [] -> parse q inp
                  [(v,out)] -> [(v,out)])

item :: Parser Char
item = P (\inp ->
            case inp of
              [] -> []
              (c:str) -> [(c,str)])

  -- Can I construct a type such that SatParser a must
  -- be derived from sat (see digit below)
sat :: (Char -> Bool) -> Parser Char
sat f = P (\inp ->
             case parse item inp of
               [] -> []
               [(c,str)] -> if (f c) then [(c,str)] else [])

digit :: Parser Char
digit = sat isDigit

until :: (Char -> Bool) -> Parser Char
until p = P (\inp -> case parse item inp of
                       [] -> []
                       [(c,str)] -> if (p c) then [(c,str)] else (parse (Parsing.until p) str))

untilDigit :: Parser Char
untilDigit = Parsing.until isDigit

lastDigit :: Parser Char
lastDigit = P (\inp -> case parse untilDigit inp of
                  [] -> []
                  [(c,str)] -> case parse untilDigit str of
                                 [] -> [(c,str)]
                                 _ -> parse lastDigit str)

testParsingLib :: IO ()
testParsingLib = do
  print (parse untilDigit "89")
  print (parse lastDigit "9")

