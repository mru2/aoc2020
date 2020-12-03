module Day02 where

import Common
import Data.Maybe (mapMaybe)
import Text.Regex.PCRE

solution =
  Solution
    { parse = mapMaybe parseLine . lines,
      part1 = countAll isValid,
      part2 = countAll isValid2
    }

data Row = Row {pos1 :: Int, pos2 :: Int, char :: Char, pwd :: String} deriving (Show)

parseLine :: String -> Maybe Row
parseLine str = case match of
  [] -> Nothing
  [[_, min, max, char, pwd]] -> Just Row {pos1 = read min, pos2 = read max, char = head char, pwd = pwd}
  where
    match = str =~ "(\\d+)-(\\d+) (\\w): (\\w+)"


isValid :: Row -> Bool
isValid Row {pos1 = pos1, pos2 = pos2, char = char, pwd = pwd} = (count >= pos1) && (count <= pos2) where count = length . filter (char ==) $ pwd

isValid2 :: Row -> Bool
isValid2 Row {pos1 = pos1, pos2 = pos2, char = char, pwd = pwd} = is_match pos1 /= is_match pos2 where is_match pos = pwd !! (pos - 1) == char