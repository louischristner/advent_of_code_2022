module Utils (rmdups, getdup, compareSnd) where

import Data.Set (toList, fromList)

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
