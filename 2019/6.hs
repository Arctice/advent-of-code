import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- input = do
--   s <- readFile "6.input"
--   let l = map (\n -> (take 3 n, drop 4 n)) (lines s)
--   return l

input = readFile "6.input" >>=
  (\l -> return
    (map (\n -> (take 3 n, drop 4 n)) $ lines l))

t1 = [("COM","B"), ("B","C"), ("C","D"), ("D","E"), ("E","F"), ("B","G"), ("G","H"), ("D","I"), ("E","J"), ("J","K"), ("K","L")]

make_tree :: [(String, String)] -> Map.Map String [String]
make_tree edges = foldl' add_edge Map.empty edges
  where add_edge tree new_edge =
          let (key, val) = new_edge
              prev = Map.lookup key tree
              edges = case prev of Just e -> (e ++ [val])
                                   Nothing -> [val]
          in Map.insert key edges tree

count_ :: Map.Map String [String] -> String -> Int -> Int
count_ g node n = let edges = case Map.lookup node g of Just e -> e
                                                        Nothing -> []
                      counts = map (\e -> count_ g e (succ n)) edges
                     in foldl (+) n counts
count g = count_ g "COM" 0
                   

test = (count.make_tree) t1

tree = fmap make_tree input

part1 = fmap (count.make_tree) input

---

add_oneway graph new_edge =
          let (key, val) = new_edge
              prev = Map.lookup key graph
              edges = case prev of Just e -> (e ++ [val])
                                   Nothing -> [val]
          in Map.insert key edges graph
graph_add_edge graph (a,b) = add_oneway (add_oneway graph (a,b)) (b,a)

make_graph :: [(String, String)] -> Map.Map String [String]
make_graph edges = foldl' graph_add_edge Map.empty edges

graph_bfs graph start end = bfs_recur [(start, 0)] Set.empty
  where bfs_recur queue seen =
          let (node, n) = head queue
              in case Set.member node seen of
                   True -> bfs_recur (tail queue) seen
                   False -> case end == node of
                     True -> n
                     False -> let out = case Map.lookup node graph of Just e -> e
                                                                      Nothing -> []
                                  more = queue ++ map (\s -> (s, (succ n))) out
                                  in bfs_recur more (Set.insert node seen)
                                  

part2 = do
  g <- input
  return $ (graph_bfs (make_graph g) "YOU" "SAN") - 2
