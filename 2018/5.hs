import qualified Data.Char as Str

input :: IO String
input = fmap (filter (not.Str.isSpace)) (readFile "5.input")

annihilate a b = (Str.toUpper a) == (Str.toUpper b) && a /= b

react :: String -> String
react poly = sub_react poly ""
  where sub_react (a:b:c) s =
          case annihilate a b of
            True -> if null s then sub_react c ""
                    else sub_react ([last s]++c) (take (length s - 1) s)
            False -> sub_react ([b]++c) (s++[a])
        sub_react a s = s ++ a

part1 = fmap (length . react) input

all_cuts poly = [cut poly a | a <- ['A'..'Z']]
  where cut poly a = filter (\c -> Str.toUpper c /= a) poly

part2 = do
  poly <- input
  let shortest = foldl1 min $ map (length . react) $ all_cuts poly
  return shortest


t = "dabAcCaCBAcCcaDA"
