act :: String -> Integer -> Integer -> (Integer, Integer, Integer)
act "forward" x a = (x, a*x, 0 )
act "down"    x a = (0, 0  , x )
act "up"      x a = (0, 0  , -x)
act _         _ _ = undefined

main :: IO()
main = interact $
  (\ans -> ans ++ "\n")
  .show
  .(\(x, y, z) -> x*y)
  .foldl (\(cx, cy, cz) (c, x) -> combine (act c x cz) (cx, cy, cz)) (0, 0, 0)
  .map (\[x, y] -> (x, read y))
  .map words
  .lines
  where combine (nx, ny, nz) (cx, cy, cz) = (cx+nx, cy+ny, cz+nz)