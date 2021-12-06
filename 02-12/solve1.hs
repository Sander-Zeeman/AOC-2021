act :: String -> Integer -> (Integer, Integer)
act "forward" x = (x, 0)
act "down"    x = (0, x)
act "up"      x = (0, -x)
act _         _ = undefined

main :: IO()
main = interact $
  (\ans -> ans ++ "\n")
  .show
  .(\(x, y) -> x*y)
  .foldl (\(cx, cy) (c, x) -> combine (act c x) (cx, cy)) (0, 0)
  .map (\[x, y] -> (x, read y))
  .map words
  .lines
  where combine (nx, ny) (cx, cy) = (cx+nx, cy+ny)