module Day03 where

import Common

solution =
  Solution
    { parse = map cycle . lines,
      part1 = treeCount (3, 1),
      part2 = product . applyAll treeCount [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    }

treeCount :: (Int, Int) -> [[Char]] -> Int
treeCount (right, down) treeMap = length . filter ('#' ==) . zipWith (!!) (filterNth down treeMap) $ iterate (right +) 0
