solve :: [Integer] -> [Integer]
solve xs = filter (> 0) $ zipWith (-) (tail xs) (init xs)

main :: IO()
main = interact $
  show
  .length
  .solve
  .map read
  .words