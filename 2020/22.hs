import Data.Tuple
import qualified Data.Set as Set
import Text.Read
import Data.Maybe

parse :: String -> ([Int], [Int])
parse ss = (numbers (fst decks), numbers (snd decks))
  where decks = (span (/= "") $ lines ss)
        numbers = catMaybes . (map readMaybe)
input = fmap parse (readFile "22.input")


win a b = (tail a ++ [head a, head b], tail b)
score (a, b) = sum $ map (uncurry (*)) (zip [1..] (reverse (a ++ b)))

clash (a, b)
  | head a > head b = win a b
  | otherwise = swap (win b a)

game (a, b)
  | null a || null b = (a, b)
  | otherwise = game (clash (a, b))

-- 30197
part1 = fmap (score . game) input

        
shahrazad history (a, b)
  | null a || null b = (a, b)
  | Set.member (a, b) history = ([1], [])
  | otherwise =
    (shahrazad (Set.insert (a, b) history) 
      (if head a < length a && head b < length b then
         case shahrazad Set.empty (take (head a) (tail a),
                                   take (head b) (tail b)) of
           (_, []) -> win a b
           ([], _) -> swap (win b a)
       else clash (a, b)))

-- 34031   
part2 = fmap (score . shahrazad Set.empty) input

