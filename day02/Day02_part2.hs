module Main where

data Move = Rock | Paper | Scissors deriving (Eq)

data Strategy = Lose | Draw | Win

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
getMoveFromString _ = Nothing

getStrategyFromString :: String -> Maybe Strategy
getStrategyFromString "X" = Just Lose
getStrategyFromString "Y" = Just Draw
getStrategyFromString "Z" = Just Win
getStrategyFromString _ = Nothing

getMatchMoveScore :: Maybe Move -> Maybe Strategy -> Int
-- draw
getMatchMoveScore move (Just Draw) = (+) 3 $ getMoveScore move
-- victories
getMatchMoveScore (Just Rock) (Just Win) = (+) 6 $ getMoveScore $ Just Paper
getMatchMoveScore (Just Paper) (Just Win) = (+) 6 $ getMoveScore $ Just Scissors
getMatchMoveScore (Just Scissors) (Just Win) = (+) 6 $ getMoveScore $ Just Rock
-- losses
getMatchMoveScore (Just Paper) (Just Lose) = getMoveScore $ Just Rock
getMatchMoveScore (Just Scissors) (Just Lose) = getMoveScore $ Just Paper
getMatchMoveScore (Just Rock) (Just Lose) = getMoveScore $ Just Scissors
-- error handling
getMatchMoveScore _ _ = 0

getMatchScore :: [String] -> Int
getMatchScore [] = 0
getMatchScore (_:[]) = 0
getMatchScore (fstVal:sndVal:_) = getMatchMoveScore fstScore strategy
  where
    fstScore = getMoveFromString fstVal
    strategy = getStrategyFromString sndVal

solve :: [String] -> Int
solve = sum . map getMatchScore . map words

main :: IO ()
main = putStrLn . (++) "Total Score: " . show . solve . splitOn 0 "\n" =<< readFile "input.txt"
