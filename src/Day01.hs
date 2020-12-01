module Day01 where

import Common
import Data.List

solution =
  Solution
    { parse = map read . lines,
      part1 = product . head . productPairs 2020,
      part2 = product . head . productTriples 2020
    }

productPairs :: Integer -> [Integer] -> [[Integer]]
productPairs target list = [[i, j] | (i : rest) <- tails list, j <- rest, i + j == target]

productTriples :: Integer -> [Integer] -> [[Integer]]
productTriples target list = [[i, j, k] | (i : is) <- tails list, (j : js) <- tails is, k <- js, i + j + k == target]
