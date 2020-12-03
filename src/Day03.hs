module Day03 where

import Common

solution =
  Solution
    { parse = map cycle . lines,
      part1 = treeCount 3 1,
      part2 = \input -> product ( map (\(right, down) -> treeCount right down input) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)] )
    }

filterNth :: Int -> [a] -> [a]
filterNth step (xs:rest) = xs:filterNth step filtered_rest where filtered_rest = drop (step - 1) rest
filterNth _ [] = []

treeCount :: Int -> Int ->  [[Char]] -> Int
treeCount right down treeMap = length . filter ('#' ==) . zipWith (!!) (filterNth down treeMap) $ iterate (right +) 0
