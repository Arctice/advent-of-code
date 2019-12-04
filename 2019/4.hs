import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Maybe as Maybe

split :: String -> Char -> [String]
split s sep = aux s [] []
  where aux [] word groups = if null word then groups else groups ++ [word]
        aux (h:t) word groups = case h == sep of
          True -> aux t [] (if null word then groups else groups ++ [word])
          False -> aux t (word ++ [h]) groups

ensure_monotonic :: Int -> Int
ensure_monotonic n = read (make_monotonic (show n) 0)
  where make_monotonic (c:s) maxc = let maxt = max maxc (read [c])
                                in (show maxt !! 0) : (make_monotonic s maxt)
        make_monotonic [] _ = []

first_repeat :: Ord a => [a] -> Maybe a
first_repeat xs = aux xs Set.empty
  where aux (x:xs) seen =
          case Set.member x seen of
            True -> Just x
            False -> aux xs (Set.insert x seen)
        aux [] seen = Nothing

has_repeat l = Maybe.isJust $ first_repeat l

range = (158126,624574)

valid_passwords (min,max) = let next = next_valid min in
                              if next <= max then
                                next : (valid_passwords (next,max))
                              else
                                []
  where next_valid c = let next = ensure_monotonic (succ c)
                       in case (has_repeat $ show next) of
                            True -> next
                            False -> next_valid next

has_pair :: Ord a => [a] -> Bool
has_pair c = not . null $ filter (\n -> 2 == length n) (List.group c)

valid_passwords2 (min,max) = let next = next_valid min in
                               if next <= max then
                                 next : (valid_passwords2 (next,max))
                               else
                                 []
  where next_valid c = let next = ensure_monotonic (succ c)
                       in case has_pair $ show next of
                            True -> next
                            False -> next_valid next

part1 = length (valid_passwords range)

part2 = length (valid_passwords2 range)
