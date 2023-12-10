module AoC1 (aoc1) where

import Control.Applicative
import Parsing
import AoCLib
import Data.Char

parseChar :: Char -> Parser Char
parseChar c = P (\inp -> case parse item inp of
                  [] -> []
                  [(x,str)] -> if (c==x) then [(x,str)] else [])


parseWord :: String -> Parser String
parseWord [] = return []
parseWord (x:xs) =
  (parseChar x) >>= (\i ->
                        parseWord xs >>= (\is -> return (x:xs)))

parseWordToInt :: Parser String -> Integer -> Parser Integer
parseWordToInt word int = P (\inp -> case parse word inp of
                                [] -> []
                                [(s,rest)] -> [(int,(tail s) ++ rest)])

intDigit :: Parser Integer
intDigit = digit >>= (\c -> return (read (c:[]) :: Integer))

fullLexer :: Parser Integer
fullLexer =
  (parseWordToInt (parseWord "one") 1) <|>
  (parseWordToInt (parseWord "two") 2) <|>
  (parseWordToInt (parseWord "three") 3) <|>
  (parseWordToInt (parseWord "four") 4) <|>
  (parseWordToInt (parseWord "five") 5) <|>
  (parseWordToInt (parseWord "six") 6) <|>
  (parseWordToInt (parseWord "seven") 7) <|>
  (parseWordToInt (parseWord "eight") 8) <|>
  (parseWordToInt (parseWord "nine") 9) <|>
  intDigit

partialLexer :: Parser Integer
partialLexer = intDigit

getInts :: String -> [Integer] -> Parser Integer -> [Integer]
getInts "" acc _ = acc
getInts str acc lexer = case parse lexer (str) of
  [] -> getInts (tail str) acc lexer
  [(c,rest)] -> getInts rest (acc ++ [c]) lexer

calibrationValue :: String -> Parser Integer -> Integer
calibrationValue str lexer = case getInts (lowercase str) [] lexer of
  [] -> 0
  xs -> read ((show (head xs)) ++ (show (last xs)))

calibrationValueReOrder :: Parser Integer -> String -> Integer
calibrationValueReOrder lexer strs = calibrationValue strs lexer

convertToInts :: [String] -> Parser Integer -> [Integer]
convertToInts [] _ = []
convertToInts strs lexer = map (calibrationValueReOrder lexer) strs

addAllCalibrationValues :: [String] -> Parser Integer -> Integer
addAllCalibrationValues strs lexer = foldr (+) 0 (convertToInts strs lexer)

lowercase :: String -> String
lowercase str = map (toLower) str

aoc1 :: IO ()
aoc1 = do
  content <- (turnFileStringIntoInputArray "/Users/brandonlara/work/AoC-in-Haskell/aoc/data/aoc1.txt")
  let x = addAllCalibrationValues (lines content) partialLexer
      y = addAllCalibrationValues (lines content) fullLexer in
    print ("Partial: " ++ (show x) ++ "  ::::    Full: " ++ (show y))
  aoc1testing (lines content)

-- Debug Functions

appendAnswer :: String -> String
appendAnswer str = str ++ " :: " ++ (show (calibrationValue str fullLexer)) ++ "\n"

appendAllAnswers :: [String] -> [String]
appendAllAnswers = map appendAnswer

concatAllStrings :: [String] -> String
concatAllStrings = foldr (++) ""

aoc1testing :: [String] -> IO ()
aoc1testing strs = do
  writeFile "/Users/brandonlara/work/AoC-in-Haskell/aoc/data/debugData2.txt" (concatAllStrings (appendAllAnswers strs))
  print (parse fullLexer "onetwo12three")
  print (calibrationValue "2oneight" fullLexer)
  print (calibrationValue "two1nine" fullLexer)
  print (calibrationValue "eighttwothree" fullLexer)
  print (calibrationValue "abcone2threexyz" fullLexer)
  print (calibrationValue "xtwone3four" fullLexer)
  print (calibrationValue "4nineeightseven2" fullLexer)
  print (calibrationValue "zoneight234" fullLexer)
  print (calibrationValue "7pqrstsixteen" fullLexer)
