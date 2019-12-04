import qualified Data.List as List

input :: IO [String]
input = fmap lines $ readFile "2.input"

grouped = List.group . List.sort

has_len n s = any (== n) $ map length s

part1 = do
  counts <- fmap grouped input
  let twos = filter (has_len 2) counts
  let threes = filter (has_len 3) counts
  return $ (length twos) + (length threes)

column_diff :: [Char] -> [Char] -> ([Char], [Char])
column_diff v u = aux v u "" ""
  where aux [] [] diff same = (diff, same)
        aux (vc:vr) (uc:ur) diff same =
          case vc == uc of
            True  -> aux vr ur diff (same ++ [vc])
            False -> aux vr ur (diff ++ [vc]) same

part2 = do
  ids <- input
  let diffs = [(column_diff a b) | a <- ids, b <- ids]
  let valid = filter (\(a,b) -> (length a) == 1) diffs
  return valid
