-- import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

input :: IO [String]
input = fmap lines $ readFile "3.input"

t = ["#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"]

split :: String -> Char -> [String]
split s sep = aux s [] []
  where aux [] word groups = if null word then groups else groups ++ [word]
        aux (h:t) word groups = case h == sep of
          True -> aux t [] (if null word then groups else groups ++ [word])
          False -> aux t (word ++ [h]) groups

pair :: [a] -> (a, a)
pair l = (head l, head $ tail l)

data Claim = Claim {claimid :: Int, pos :: (Int, Int), size :: (Int, Int)}
parse_claim :: String -> Claim
parse_claim s = Claim (read id) (x, y) (w, h)
  where (id, d) = pair $ split (tail s) '@'
        (pos, size) = pair $ split d ':'
        (x, y) = pair $ map read $ split pos ','
        (w, h) = pair $ map read $ split size 'x'

area :: Claim -> Set.Set (Int, Int)
area c = Set.fromList
         [(x,y) | x <- [fst $ pos c .. pred $ (fst $ pos c) + (fst $ size c)],
                  y <- [snd $ pos c .. pred $ (snd $ pos c) + (snd $ size c)]]

combine (grid, overlap) claim = (new_grid, Set.union overlap new_overlap)
  where new_grid = Set.union grid claim
        new_overlap = Set.intersection grid claim

overlaps :: [Claim] -> Set.Set (Int, Int)
overlaps claims = overlap
  where areas = map area claims
        (grid, overlap) = foldl combine (Set.empty, Set.empty) areas

result = do
  d <- input
  let claims = map parse_claim d
  let overlap = overlaps claims
  let disjoint = fst $ head $
        filter (\(id,c) -> Set.disjoint overlap c) (map (\c -> (claimid c, area c)) claims)
  return $ (length overlap, disjoint)
