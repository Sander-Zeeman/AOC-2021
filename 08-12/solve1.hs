main :: IO()
main = interact
  $ (\x -> x ++ "\n")
  .show
  .sum
  .map length
  .map (filter (\x -> elem x [2,3,4,7]))
  .map (map length)
  .map tail
  .map (dropWhile ("|" /=))
  .map words
  .lines