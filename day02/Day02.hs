module Main where

data Move = Rock | Paper | Scissors deriving (Eq)

-- utils

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

-- main

getMoveScore :: Maybe Move -> Int
getMoveScore (Just Rock) = 1
getMoveScore (Just Paper) = 2
getMoveScore (Just Scissors) = 3
getMoveScore Nothing = 0

getMoveFromString :: String -> Maybe Move
getMoveFromString "A" = Just Rock
getMoveFromString "B" = Just Paper
getMoveFromString "C" = Just Scissors
getMoveFromString "X" = Just Rock
getMoveFromString "Y" = Just Paper
getMoveFromString "Z" = Just Scissors
getMoveFromString _ = Nothing

getMatchMoveScore :: Maybe Move -> Maybe Move -> Int
-- draw
getMatchMoveScore fstMove sndMove | fstMove == sndMove = (+) 3 $ getMoveScore sndMove
-- losses
getMatchMoveScore (Just Rock) (Just Paper) = (+) 6 $ getMoveScore $ Just Paper
getMatchMoveScore (Just Paper) (Just Scissors) = (+) 6 $ getMoveScore $ Just Scissors
getMatchMoveScore (Just Scissors) (Just Rock) = (+) 6 $ getMoveScore $ Just Rock
-- victories
getMatchMoveScore (Just Paper) (Just Rock) = getMoveScore $ Just Rock
getMatchMoveScore (Just Scissors) (Just Paper) = getMoveScore $ Just Paper
getMatchMoveScore (Just Rock) (Just Scissors) = getMoveScore $ Just Scissors
-- error handling
getMatchMoveScore _ _ = 0

getMatchScore :: [String] -> Int
getMatchScore [] = 0
getMatchScore (_:[]) = 0
getMatchScore (fstVal:sndVal:_) = getMatchMoveScore fstScore sndScore
  where
    fstScore = getMoveFromString fstVal
    sndScore = getMoveFromString sndVal

solve :: [String] -> Int
solve = sum . map getMatchScore . map words

main :: IO ()
main = putStrLn . (++) "Total Score: " . show . solve . splitOn 0 "\n" =<< readFile "input.txt"
