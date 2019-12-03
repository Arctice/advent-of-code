import qualified Data.IntSet as Set
import qualified Data.Vector as Vec

input :: IO [String]
input = fmap lines $ readFile "1.input"

strip_plus :: String -> String
strip_plus ('+':rest) = rest
strip_plus str = str

part1 :: IO Int
part1 = do
  ls <- input
  let xs =  map (\s -> read $ strip_plus s) ls
  let result = sum xs
  return result


first_repeated :: [Int] -> Maybe Int
first_repeated xs = aux xs Set.empty
  where aux (x:xs) seen =
          case Set.member x seen of
            True -> Just x
            False -> aux xs (Set.insert x seen)
        aux [] seen = Nothing

part2 = do
  ls <- input
  let changes =  map (\s -> (read $ strip_plus s) :: Int) ls
  let series = cycle changes
  let values = scanl (+) 0 series
  return $ first_repeated values

part2alt :: Vec.Vector Int -> Maybe Int
part2alt xs = aux (Set.insert 0 Set.empty) 0 0 where
  aux :: Set.IntSet -> Int -> Int -> Maybe Int
  aux seen i s =
    let v = s + (xs Vec.! i) 
        in case Set.member v seen of
      True -> Just v
      False -> (aux (Set.insert v seen) next v)
        where next = mod (succ i) (length xs)

part2i = do
  ls <- input
  let changes = map (\s -> (read $ strip_plus s) :: Int) ls
  let changes_vec = Vec.generate (length changes) (\i -> changes !! i)
  return $ part2alt changes_vec
