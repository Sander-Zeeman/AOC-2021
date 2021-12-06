fromBool :: [Bool] -> Integer
fromBool [] = 0
fromBool (b:bs) = boolToInt b + 2 * fromBool bs
  where
    boolToInt :: Bool -> Integer
    boolToInt False = 0
    boolToInt True  = 1

decide :: [Bool] -> Bool
decide bs
  | count bs >= 0 = True
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

find :: [(Integer, [Bool])] -> Bool -> Integer
find ((i, _):[]) _ = i
find bs inv = find ((map (\(i, b) -> (i, tail b)) . filter (\(i, b) -> head b == g bs)) bs) inv
  where
    g bs
      | inv = not $ head $ map decide $ transpose $ map snd bs
      | otherwise = head $ map decide $ transpose $ map snd bs

massFilter :: [[Bool]] -> (Integer, Integer)
massFilter bs = (fromBool $ reverse (bs !! i1), fromBool $ reverse (bs !! i2))
  where
    i1 = fromIntegral $ find (zip [0..] bs) False
    i2 = fromIntegral $ find (zip [0..] bs) True

main :: IO ()
main = interact $
  show
  .(\(a,b) -> a * b)
  .massFilter
  .map toBool
  .lines