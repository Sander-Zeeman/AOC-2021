combine :: [Integer] -> [Integer] -> [Integer]
combine (x:xs) (y:ys) = scanl (\z (nx, ny) -> ny + min z nx) (x + y) (zip xs ys)

finishMap :: [[Integer]] -> [[Integer]]
finishMap board = full
  where
    step :: [[Integer]] -> [[Integer]]
    step = map $ map (\x -> 1 + mod x 9)

    horFields = take 5 $ iterate step board
    fullHor = [concat $ map (!! i) horFields | i <- [0..(length board) - 1]]

    fullFields = take 5 $ iterate step fullHor
    full = concat fullFields

main :: IO()
main = interact
  $ (\x -> x ++ "\n")
  . show
  . (\((x:xs):ys) -> (foldl combine (scanl (+) x xs) ys))
  . reverse
  . map reverse
  . finishMap
  . map (map read)
  . map (map (:[]))
  . lines