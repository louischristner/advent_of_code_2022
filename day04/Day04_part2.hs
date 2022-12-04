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

readInt :: String -> Int
readInt = read

getTupleFromString :: String -> String -> (String, String)
getTupleFromString del str = (xsFst, xsSnd)
  where
    xsFst = head xs
    xsSnd = head $ tail xs
    xs = splitOn 0 del str

intersect :: [Int] -> [Int] -> [Int]
intersect [] = const []
intersect xs = filter (`elem` xs)

solve :: [((Int, Int), (Int, Int))] -> Int
solve xs = length $ filter (\x -> 0 < length x) intersects
  where
    intersects = map (\(x, y) -> intersect x y) sections
    sections = map (\((x1, x2), (y1, y2)) -> ([x1..x2], [y1..y2])) xs

main :: IO ()
main = putStrLn
  . show
  . solve
  . map (\((x1, x2), (y1, y2)) -> ((readInt x1, readInt x2), (readInt y1, readInt y2)))
  . map (\(x, y) -> (getTupleFromString "-" x, getTupleFromString "-" y))
  . map (getTupleFromString ",")
  . lines =<< readFile "input.txt"