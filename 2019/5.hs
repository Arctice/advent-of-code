import qualified Debug.Trace as DT

csplit str = aux str "" [] where
  aux (',':rest) word words = aux rest "" $ words ++ [word]
  aux (ch:rest) word words = aux rest (word ++ [ch]) words
  aux [] word words = words ++ [word]

input :: IO [Int]
input = fmap (\s -> map read (csplit s)) (readFile "5.input")

tape_set :: [Int] -> Int -> Int -> [Int]
tape_set tape dest v = let (l,r) = splitAt dest tape in l ++ [v] ++ (tail r)

tape_read tape pos mode = let read = tape !! pos
                              in case mode of
                                   0 -> tape !! read
                                   1 -> read

tape_arithmetic op tape op_pos modes = tape_set tape dest val
  where a = tape_read tape (op_pos + 1) (modes !! 0)
        b = tape_read tape (op_pos + 2) (modes !! 1)
        val = op a b
        dest = tape !! (op_pos + 3)

tape_add = tape_arithmetic (+)
tape_mul = tape_arithmetic (*)
tape_less_than = tape_arithmetic (\a b -> if a < b then 1 else 0)
tape_equals = tape_arithmetic (\a b -> if a == b then 1 else 0)

-- tape_write :: [Int] -> Int -> Int -> [Int]
tape_write tape op_pos val = tape_set tape dest val
  where dest = tape !! (op_pos + 1)

jump_if pred tape pos modes = let val = tape_read tape (pos + 1) (modes !! 0)
                                  jump = pred val
                              in case jump of
                                True -> Just (tape_read tape (pos + 2) (modes !! 1))
                                False -> Just (pos + 3)

jump_ifnz = jump_if (\n -> n /= 0)
jump_ifz  = jump_if (\n -> n == 0)

decode code = let op = mod code 100
                  c = mod (quot code 100) 10
                  b = mod (quot code 1000) 10
              in (op, [c, b])

step tape pos input output = let (op, modes) = decode (tape !! pos)
  in case op of
       99 -> (tape, input, output, Nothing) -- halt
       1 -> (tape_add tape pos modes, input, output, Just (pos + 4))
       2 -> (tape_mul tape pos modes, input, output, Just (pos + 4))
       3 -> (tape_write tape pos (head input), tail input, output, Just (pos + 2))
       4 -> (tape, input, output ++ [tape_read tape (pos+1) (head modes)], Just (pos + 2))
       5 -> (tape, input, output, jump_ifnz tape pos modes)
       6 -> (tape, input, output, jump_ifz tape pos modes)
       7 -> (tape_less_than tape pos modes, input, output, Just (pos + 4))
       8 -> (tape_equals tape pos modes, input, output, Just (pos + 4))

run tape pos input output = let (state, inp, out, next) = step tape pos input output in
                              case next of
                                Just op -> run state op inp out
                                Nothing -> (state, out)

part1 = do
  s <- input
  let z = run s 0 [1] []
  return z

part2 = do
  s <- input
  let z = run s 0 [5] []
  return z
