combine :: [Integer] -> [Integer] -> [Integer]
combine (x:xs) (y:ys) = scanl (\z (nx, ny) -> ny + min z nx) (x + y) (zip xs ys)

main :: IO()
main = interact
  $ (\x -> x ++ "\n")
  . show
  . (\((x:xs):ys) -> scanl combine (scanl (+) x xs) ys)
  . reverse
  . map reverse
  . map (map read)
  . map (map (:[]))
  . lines