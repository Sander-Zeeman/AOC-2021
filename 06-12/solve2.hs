step :: [Integer] -> [Integer]
step [a,b,c,d,e,f,g,h,i] = [b,c,d,e,f,g,h + a,i,a] 

specificCount :: [Integer] -> Integer -> Integer
specificCount [] _ = 0
specificCount (f:fs) x
  | f == x = 1 + specificCount fs x
  | otherwise = specificCount fs x

count :: [Integer] -> [Integer]
count fs = map (specificCount fs) [0..8]

main :: IO()
main = interact
  $ (\x -> x ++ "\n")
  .show
  .sum
  .head
  .drop 256
  .iterate step
  .count
  .map read
  .words
