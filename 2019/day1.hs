import System.IO

fuel :: Int -> Int
fuel mass = mass `div` 3 - 2

-- calculate fuel for fuel as well
fuel' :: Int -> Int
fuel' mass
  | f <= 0    = 0
  | otherwise = f + fuel' f
  where f = fuel mass

main = do
  contents <- readFile "input1.txt"
  let masses = map read $ lines contents

  putStrLn $ show $ sum $ map fuel masses
  putStrLn $ show $ sum $ map fuel' masses
