fromBool :: [Bool] -> Integer
fromBool [] = 0
fromBool (b:bs) = boolToInt b + 2 * fromBool bs
  where
    boolToInt :: Bool -> Integer
    boolToInt False = 0
    boolToInt True  = 1

decide :: [Bool] -> Bool
decide bs
  | count bs > 0 = True
  | otherwise    = False
  where
    count [] = 0
    count (b:bs)
      | b = count bs + 1
      | otherwise = count bs - 1

transpose :: [[Bool]] -> [[Bool]]
transpose bs = [map (!! k) bs | k <- [0..(fromIntegral . length . head) bs - 1]]

toBool :: String -> [Bool]
toBool [] = []
toBool (c:cs)
  | c == '1' = True : toBool cs
  | c == '0' = False : toBool cs

main :: IO ()
main = interact $
  show
  .(\xs -> fromBool xs * fromBool (map not xs))
  .reverse
  .map decide
  .transpose
  .map toBool
  .lines