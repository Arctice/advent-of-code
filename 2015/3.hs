import qualified Data.List as List

data Vec2 = Vec2 Int Int deriving (Eq, Ord, Show)

vec2Add (Vec2 x y) (Vec2 x' y') = Vec2 (x + x') (y + y')

char_dir c = case c of
  '^' -> Vec2 0 (-1); '>' -> Vec2 1 0;
  'v' -> Vec2 0 1; '<' -> Vec2 (-1) 0;

move p c = vec2Add p (char_dir c)
deliveries dirs = foldl traverse [Vec2 0 0] dirs
  where traverse (pos:path) step = (move pos step):pos:path

count_presents = length . List.nub

part1 = count_presents . deliveries

part2 is = count_presents ((deliveries a) ++ (deliveries b))
  where parts = List.partition (\ (v, i) -> even i) (zip is [0..])
        a = map fst $ fst parts
        b = map fst $ snd parts

main = do input <- readFile "3.input"
          print $ part1 input
          print $ part2 input
