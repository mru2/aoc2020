{-# LANGUAGE OverloadedStrings #-}

module Common where

import Control.Exception
import System.Clock
import Formatting
import Formatting.Clock
import Text.Printf


data Solution a b c =
    Solution
        { parse :: String -> a
        , part1 :: a -> b
        , part2 :: a -> c
        }

benchmark :: IO a -> IO ()
benchmark action = do
    start <- getTime Monotonic
    action
    end <- getTime Monotonic
    fprint (" (" % timeSpecs % ")\n") start end

aoc :: (Show b, Show c) => Int -> Solution a b c -> IO ()
aoc n solution = do
    input <- readFile $ printf "data/%02d" n ++ ".txt"
    let inputWithoutNewline = init input
    let problem = parse solution inputWithoutNewline
    benchmark $ do
        putStr "Parsing input..."
        evaluate problem
    benchmark $ putStr $ "Part 1: " ++ show (part1 solution problem)
    benchmark $ putStr $ "Part 2: " ++ show (part2 solution problem)

tbd :: a -> String
tbd _ = "(not implemented)"