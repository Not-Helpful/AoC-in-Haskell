module Minimum (broken) where

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse _ [] = []
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

sat :: (Char -> Bool) -> Parser Char
sat f = P (\inp ->
             case parse item inp of
               [] -> []
               [(c,str)] -> if (f c) then [(c,str)] else [])

digit :: Parser Char
digit = sat isDigit


intDigit :: Parser Integer
intDigit = digit >>= (\c -> return (read (c:[]) :: Integer))


broken :: IO ()
broken = do
  print (parse intDigit "41")
  print (parse intDigit "1")
