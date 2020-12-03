module Day03 where

import Common

solution =
  Solution
    { parse = map cycle . lines,
      part1 = treeCount 3,
      part2 = tbd
    }

treeCount :: Int -> [[Char]] -> Int
treeCount slope treeMap = countAll ('#' ==) . zipWith (!!) treeMap $ iterate (slope +) 0
