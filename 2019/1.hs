-- input = readFile "aoc1.input" >>= \str -> return (lines str)
-- input = fmap lines $ readFile "aoc1.input"

input :: IO [Int]
input = do
  str <- readFile "1.input"
  let ls = map read . lines $ str
  return ls

fuel :: Int -> Int
fuel m = max 0 $ floor (fromIntegral(m) / 3) - 2

rocket_fuel :: Int -> Int
rocket_fuel m = sum $ drop 1 $
                takeWhile (\x -> x > 0) overhead where
  overhead = m : [fuel y | y <- overhead]

part1 = do
  modules <- input
  let fueled = fmap fuel modules
  return (sum fueled)

part2 = do
  modules <- input
  let fueled = fmap rocket_fuel modules
  return (sum fueled)

main = part1 >>= \a -> print a
    >> part2 >>= \b -> print b
