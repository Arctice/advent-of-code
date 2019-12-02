input = readFile "2.input"

csplit str = aux str "" [] where
  aux (',':rest) word words = aux rest "" $ words ++ [word]
  aux (ch:rest) word words = aux rest (word ++ [ch]) words
  aux [] word words = words ++ [word]


tape_set (x:rest) 0 v = v : rest
tape_set tape dest v = (head tape) : tape_set (tail tape) (pred dest) v

tape_op op tape src1 src2 dest = let val = op (tape !! src1) (tape !! src2)
                                 in tape_set tape dest val

op_add = tape_op (+)
op_mul = tape_op (*)

t1 :: [Int]
t1 = [1,9,10,3,2,3,11,0,99,30,40,50]

p1step tape pos = let op = tape !! pos in
  if op == 99 then (tape, Nothing) else
    let src1 = tape !! (1 + pos)
        src2 = tape !! (2 + pos)
        dest = tape !! (3 + pos)
        next = 4 + pos
    in case op of
      1 -> (op_add tape src1 src2 dest, Just next)
      2 -> (op_mul tape src1 src2 dest, Just next)

p1run :: [Int] -> Int -> [Int]
p1run tape pos = let (state, next) = p1step tape pos in
                   case next of
                     Just op -> p1run state op
                     Nothing -> state

adjust tape noun verb = head tape : noun : verb : drop 3 tape

p1 a b = do
  i <- input
  let d = map read (csplit i) :: [Int]
  let init = adjust d a b
  let result = p1run init 0
  return $ result !! 0

part1 = p1 12 2

p2check tape noun verb = p1run (adjust tape noun verb) 0

goal = 19690720

part2 = do
  i <- input
  let d = map read (csplit i) :: [Int]
  let options = [(a,b) | a<-[0..99], b<-[0..99]]
  let search = filter (\(a,b) -> (head $ (p2check d a b)) == goal) options
  return $ head search

