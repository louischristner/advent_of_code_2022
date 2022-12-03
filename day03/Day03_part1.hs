module Main where

import Utils (rmdups, getdup)
import Data.Char (ord)

import Data.List (sortBy)

compareSnd :: (Int, Int) -> (Int, Int) -> Ordering
compareSnd (_, snd1) (_, snd2) = if snd1 < snd2 then GT else LT

getCharPriority :: Maybe Char -> Int
getCharPriority Nothing = 0
getCharPriority (Just c) =
  if asciiVal <= (ord 'Z') then
    (asciiVal - (ord 'A')) + 27
  else
    (asciiVal - (ord 'a')) + 1
  where
    asciiVal = ord c

getRucksackFromString :: String -> (String, String)
getRucksackFromString val = (fstPart, sndPart)
  where
    fstPart = take halfLen val
    sndPart = drop halfLen val
    halfLen = (length val) `div` 2

solve :: [String] -> Int
solve = sum . map getCharPriority . map getdup . map (\(x, y) -> x ++ y) . map (\(x, y) -> (rmdups x, rmdups y)) . map getRucksackFromString

main :: IO ()
main = putStrLn . (++) "Sum of priorities: " . show . solve . words =<< readFile "input.txt"
