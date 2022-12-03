module Main where

import Data.Char (ord)
import Data.List (sortBy)
import Utils (rmdups, compareSnd)

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

solve :: [String] -> [Char]
solve [] = []
solve (_:[]) = []
solve (_:_:[]) = []
solve (xs1:xs2:xs3:xsTail) =
  [ fst
  $ head
  $ sortBy compareSnd
  $ map (\x -> (head x, length x))
  $ map (\x -> filter (==x) xsStr) xsStr
  ] ++ solve xsTail
  where
    xsStr = concat $ map rmdups $ [xs1,xs2,xs3]

main :: IO ()
main = putStrLn
  . (++) "Sum of priorities: "
  . show
  . sum
  . map getCharPriority
  . map (\x -> Just x)
  . words
  . lines =<< readFile "input.txt"
