module Main where

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

getNextCrate :: String -> Maybe Char
getNextCrate [] = Nothing
getNextCrate (_:[]) = Nothing
getNextCrate (_:' ':_) = Nothing
getNextCrate (_:x:_) = Just x

getLineCrates :: String -> [Maybe Char]
getLineCrates [] = []
getLineCrates line = [getNextCrate line] ++ (getLineCrates $ drop 4 line)

getCrates :: [String] -> [[Maybe Char]]
getCrates [] = []
getCrates inputs = [getLineCrates $ head inputs] ++ (getCrates $ tail inputs)

takeAllAtIndex :: Int -> [[a]] -> [a]
takeAllAtIndex _ [] = []
takeAllAtIndex idx xs = [head $ drop idx $ head xs] ++ (takeAllAtIndex idx $ tail xs)

getMaybeValue :: a -> Maybe a -> a
getMaybeValue defaultValue x =
  case x of
    Nothing -> defaultValue
    Just value -> value

getMaybeListValues :: Eq a => a -> [Maybe a] -> [a]
getMaybeListValues defaultValue = map (getMaybeValue defaultValue) . filter (\x -> x /= Nothing)

turnCrates :: Int -> Int -> [[Maybe Char]] -> [[Char]]
turnCrates idx maxIdx _ | idx == maxIdx = []
turnCrates idx maxIdx crates = [getMaybeListValues '0' $ takeAllAtIndex idx crates] ++ (turnCrates (idx + 1) maxIdx crates)

getIndexesFromMove :: [String] -> (Int, Int, Int)
getIndexesFromMove [] = (0, 0, 0)
getIndexesFromMove (_:[]) = (0, 0, 0)
getIndexesFromMove (_:_:[]) = (0, 0, 0)
getIndexesFromMove (_:_:_:[]) = (0, 0, 0)
getIndexesFromMove (_:_:_:_:[]) = (0, 0, 0)
getIndexesFromMove (_:_:_:_:_:[]) = (0, 0, 0)
getIndexesFromMove (_:amount:_:fstIdx:_:sndIdx:_) =
  (read amount, read fstIdx - 1, read sndIdx - 1)

getMoves :: [String] -> [(Int, Int, Int)]
getMoves = map getIndexesFromMove . map words

applyMoves :: [(Int, Int, Int)] -> [[Char]] -> [[Char]]
applyMoves [] crates = crates
applyMoves (move:tailMoves) crates = applyMoves tailMoves newCrates
  where
    newCrates = (++) sndLeftCrates $ (++) [newToCrates] $ tail sndRightCrates
    (sndLeftCrates, sndRightCrates) = splitAt sndIdx $ (++) fstLeftCrates $ (++) [newFromCrates] $ tail fstRightCrates
    (fstLeftCrates, fstRightCrates) = splitAt fstIdx crates
    newToCrates = wantedCrates ++ toCrates
    newFromCrates = drop amount fromCrates
    wantedCrates = take amount fromCrates
    toCrates = crates !! sndIdx
    fromCrates = crates !! fstIdx
    (amount, fstIdx, sndIdx) = move

solve :: String -> [[Char]]
solve input = applyMoves moves $ turnCrates 0 (length $ head crates) crates
  where
    moves = getMoves $ lines moveLines
    crates = getCrates $ reverse $ tail $ reverse $ lines crateLines
    crateLines = head splittedLines
    moveLines = head $ tail splittedLines
    splittedLines = splitOn 0 "\n\n" input

main :: IO ()
main = putStrLn . map head . filter (\x -> length x > 0) . solve =<< readFile "input.txt"