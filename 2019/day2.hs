import Data.List.Extra (split)

write :: Int -> Int -> [Int] -> [Int]
write pos val cs = take pos cs ++ [val] ++ drop (pos+1) cs

step :: [Int] -> Int -> (Bool, [Int])
step computer pc
  | opcode  == 99 = (False, computer)
  | opcode  == 1  = (True, write' (val1 + val2))
  | opcode  == 2  = (True, write' (val1 * val2))
  | otherwise     = (False, computer)
  where opcode  = computer!!pc
        pos1    = computer!!(pc+1)
        pos2    = computer!!(pc+2)
        pos3    = computer!!(pc+3)
        val1    = computer!!pos1
        val2    = computer!!pos2
        write'  = (\v -> write pos3 v computer)

run :: [Int] -> Int -> [Int]
run computer pc = do
    let (cont, c') = step computer pc
    if cont then
      run c' (pc+4)
    else
      c'

writeNV :: [Int] -> (Int, Int) -> [Int]
writeNV cs (n,v) = write 1 n $ write 2 v cs

result :: [Int] -> (Int, Int) -> Int
result cs (n,v) = (run cs' 0)!!0
  where cs' = write 1 n $ write 2 v cs

results :: [Int] -> [(Int, Int, Int)]
results cs = map f nvs
  where nvs = [(n,v) | n <- [0..99], v <- [0..99]]
        f   = (\(n,v) -> (n,v,result cs (n,v)))

main = do
  contents <- readFile "input2.txt"
  let computer = map read $ split (==',') contents
  -- let computer' = write 1 12 $ write 2 2 computer
  -- 3716250

  let (n,v,r) = (dropWhile (\(_,_,r) -> r /= 19690720) (results computer))!!0

  putStrLn $ show n
  putStrLn $ show v
  putStrLn $ show r
  putStrLn $ show $ n*100+v
