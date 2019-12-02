import qualified Data.Map.Strict as Map

input = readFile "2.input"

csplit str = aux str "" [] where
  aux (',':rest) word words = aux rest "" $ words ++ [word]
  aux (ch:rest) word words = aux rest (word ++ [ch]) words
  aux [] word words = words ++ [word]

goal = 19690720


tape_set (x:rest) 0 v = v : rest
tape_set tape dest v = (head tape) : tape_set (tail tape) (pred dest) v

tape_op op tape src1 src2 dest = let val = op (tape !! src1) (tape !! src2)
                                 in tape_set tape dest val

op_add = tape_op (+)
op_mul = tape_op (*)

t1 :: [Int]
t1 = [1,9,10,3,2,3,11,0,99,30,40,50]

step tape pos = let op = tape !! pos in
  if op == 99 then (tape, Nothing) else
    let src1 = tape !! (1 + pos)
        src2 = tape !! (2 + pos)
        dest = tape !! (3 + pos)
        next = 4 + pos
    in case op of
      1 -> (op_add tape src1 src2 dest, Just next)
      2 -> (op_mul tape src1 src2 dest, Just next)

run :: [Int] -> Int -> [Int]
run tape pos = let (state, next) = step tape pos in
                   case next of
                     Just op -> run state op
                     Nothing -> state

adjust tape noun verb = head tape : noun : verb : drop 3 tape

p1 a b = do
  i <- input
  let d = map read (csplit i) :: [Int]
  let init = adjust d a b
  let result = run init 0
  return $ result !! 0

part1 = p1 12 2

p2check tape noun verb = run (adjust tape noun verb) 0

part2 = do
  i <- input
  let d = map read (csplit i) :: [Int]
  let options = [(a,b) | a<-[0..99], b<-[0..99]]
  let search = filter (\(a,b) -> (head $ (p2check d a b)) == goal) options
  return $ head search

------------------

data Polynomial = Polynomial {poly :: (Map.Map (Int, Int) Int)}
instance Show Polynomial where
  show (Polynomial z) = "(" ++ foldl1 (++) (map atom (Map.toList z)) ++ ")"
    where atom ((n,v),x) = "+" ++ show x ++ sn ++ sv
            where sn = if n > 0 then ("n" ++ show n) else ""
                  sv = if v > 0 then ("v" ++ show v) else ""

singleexpr n = Polynomial (Map.fromList [((0,0),n)])
noun = Polynomial (Map.fromList [((1,0),1)])
verb = Polynomial (Map.fromList [((0,1),1)])
tape2expr (a:n:v:rest) = singleexpr a : noun : verb : (map (\n -> singleexpr n) rest)

polyadd :: Polynomial -> Polynomial -> Polynomial
polyadd a b = Polynomial $ foldl1 Map.union [only_a, only_b, intersection]
  where only_a = poly a Map.\\ poly b
        only_b = poly b Map.\\ poly a
        intersection = Map.intersectionWithKey intersect (poly a) (poly b)
          where intersect k a b = a + b

polymul :: Polynomial -> Polynomial -> Polynomial
polymul a b = Polynomial multiplied
  where pairs = [(a,b) | a <- Map.toList (poly a), b <- Map.toList (poly b)]
        multiplied = foldl mul Map.empty pairs
          where mul poly pair = Map.insert ((n1+n2,v1+v2) :: (Int,Int)) (x1*x2 :: Int) poly
                  where (((n1,v1),x1),((n2,v2), x2)) = pair

unsafe_decode :: Polynomial -> Int
unsafe_decode p = i where Just i = Map.lookup (0,0) (poly p)

unsafe_lookup :: Polynomial -> [Polynomial] -> Polynomial
unsafe_lookup p tape = case Map.lookup (0,0) (poly p) of
                         Just i -> tape !! i
                         Nothing -> case Map.lookup (1,0) (poly p) of
                           Just n -> noun
                           Nothing -> case Map.lookup (0,1) (poly p) of
                             Just v -> verb

partial_set :: [Polynomial] -> Int -> Polynomial -> [Polynomial]
partial_set (x:rest) 0 v = v : rest
partial_set tape dest v = (head tape) : partial_set (tail tape) (pred dest) v

partial_op :: (Polynomial -> Polynomial -> Polynomial) -> [Polynomial]
  -> Polynomial -> Polynomial -> Polynomial -> [Polynomial]
partial_op op tape src1_poly src2_poly dest_poly =
  partial_set tape dest val
  where src1 = unsafe_lookup src1_poly tape
        src2 = unsafe_lookup src2_poly tape
        dest = unsafe_decode dest_poly
        val = op src1 src2

partial_add = partial_op polyadd
partial_mul = partial_op polymul

partial_step tape pos = let op = unsafe_decode (tape !! pos) in
  if op == 99 then (tape, Nothing) else
    let src1 = tape !! (1 + pos)
        src2 = tape !! (2 + pos)
        dest = tape !! (3 + pos)
        next = 4 + pos
    in case op of
      1 -> (partial_add tape src1 src2 dest, Just next)
      2 -> (partial_mul tape src1 src2 dest, Just next)

partial_run :: [Polynomial] -> Int -> [Polynomial]
partial_run tape pos = let (state, next) = partial_step tape pos in
                         case next of
                           Just op -> partial_run state op
                           Nothing -> state

eval_polynomial p (n,v) = foldl (+) 0 (map eval (Map.toList (poly p)))
  where eval ((en,ev),a) = a*(n^en * v^ev)
   
-- This shouldn't have worked, some cases haven't been taken into account
-- but the input data happens to be nice enough that this didn't matter
part2_magical = do
  i <- input
  let d = map read (csplit i) :: [Int]
  let cursed = tape2expr d
  let processed = partial_run cursed 0
  let partial_answer = head processed
  let options = [(a,b) | a<-[0..99], b<-[0..99]]
  let search = filter (\(a,b) -> (eval_polynomial partial_answer (a,b)) == goal) options
  return search
