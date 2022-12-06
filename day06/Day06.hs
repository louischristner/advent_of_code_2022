module Main where

import Data.Set (toList, fromList)

rmdups :: (Ord a) => [a] -> [a]
rmdups = toList . fromList

getFirstMarkerIndex :: Int -> Int -> String -> Int
getFirstMarkerIndex _ idx [] = idx
getFirstMarkerIndex markerLength idx input =
  if length filteredChars < markerLength then
    getFirstMarkerIndex markerLength (idx + 1) $ tail input
  else
    idx
  where
    filteredChars = rmdups $ take markerLength input

solve :: Int -> String -> Int
solve markerLength = (+) markerLength . getFirstMarkerIndex markerLength 0

-- part 1
-- main :: IO ()
-- main = putStrLn . show . solve 4 =<< readFile "input.txt"

-- part 2
main :: IO ()
main = putStrLn . show . solve 14 =<< readFile "input.txt"