solve :: [Integer] -> [Integer]
solve xs = filter (> 0) $ zipWith (-) (tail sums) (init sums)
  where sums = zipWith3 (\x y z -> x+y+z) (init $ init xs) (init $ tail xs) (tail $ tail xs)

main :: IO()
main = interact $ 
  show
  .length
  .solve
  .map read
  .words