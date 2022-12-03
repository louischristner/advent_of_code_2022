module Utils (splitOn, rmdups, getdup, compareSnd) where

import Data.Set (toList, fromList)

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

rmdups :: (Ord a) => [a] -> [a]
rmdups = toList . fromList

getdup :: [Char] -> Maybe Char
getdup [] = Nothing
getdup (xsHead:xs) =
  if (length filteredXs) >= 1 then
    Just xsHead
  else
    getdup xs
  where
    filteredXs = filter (\x -> x == xsHead) xs

compareSnd :: (a, Int) -> (a, Int) -> Ordering
compareSnd (_, snd1) (_, snd2) = if snd1 < snd2 then GT else LT
