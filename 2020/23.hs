import Data.List
import Data.Maybe

-- cups = [3,8,9,1,2,5,4,6,7]
cups = [2,1,5,6,9,4,7,8,3]

move cups = let
  first = head cups
  cut = take 3 $ drop 1 $ cups
  rest = drop 4 cups
  choices = (sort $ filter (> first) rest) ++ (sort $ filter (< first) rest)
  shift = 1 + (fromJust $ elemIndex (last choices) rest)
  in (take shift rest) ++ cut ++ (drop shift rest) ++ [first]


part1 = let
  moved = (iterate move cups) !! 100
  shift = (fromJust $ elemIndex 1 moved)
  result = (drop (1 + shift) moved) ++ (take shift moved)
  in putStrLn (foldl1 (++) (map show result))

