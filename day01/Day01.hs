module Main where

import Data.List (sort)

splitOn :: Int -> String -> String -> [String]
splitOn idx _ input | idx == length input = [input]
splitOn idx del input =
  if key == del then
    [prevKey] ++ (splitOn 0 del $ drop (idx + delLen) input)
  else
    splitOn (idx + 1) del input
  where
    delLen = length del
    key = take delLen $ drop idx input
    prevKey = take idx input

solve :: [String] -> [Int]
solve = reverse . sort . map sum . map (\x -> map read x :: [Int]) . map words

-- -- first half
-- main :: IO ()
-- main = putStrLn . (++) "Top Value: " . show . head . solve . splitOn 0 "\n\n" =<< readFile "input.txt"

-- second half
main :: IO ()
main = putStrLn . (++) "Top Three Values Combined: " . show . sum . take 3 . solve . splitOn 0 "\n\n" =<< readFile "input.txt"
