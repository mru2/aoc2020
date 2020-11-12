module Day01 where

import Common

solution =
    Solution
      { parse = map read . lines,
        part1 = sum . map fuel,
        part2 = sum . map totalFuel
      }

fuel :: Integer -> Integer
fuel = max 0 . subtract 2 . (`div` 3)

totalFuel :: Integer -> Integer
totalFuel = sum . takeWhile (> 0) . tail . iterate fuel
