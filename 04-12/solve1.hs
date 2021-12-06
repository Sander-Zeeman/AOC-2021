type Board = [[(Integer, Bool)]]

score :: (Board, Integer) -> Integer
score (b, x) = x * (sum . map sum . map (map fst)) unmarked
  where unmarked = map (filter (\(v, s) -> not s)) b

transpose :: Board -> Board
transpose b = [map (!! k) b | k <- [0..4]]

horizontal :: Board -> Bool
horizontal b = any (all (snd)) b

vertical :: Board -> Bool
vertical b = any (all (snd)) (transpose b)

getWinner :: [Board] -> Integer -> Integer
getWinner [] _ = -1
getWinner (b:bs) n
  | winner = n
  | otherwise = getWinner bs (n+1)
  where
    winner = horizontal b || vertical b

markBoard :: Board -> Integer -> Board
markBoard b x = map (map (\(i, s) -> (i, s || i == x))) b

mark :: [Board] -> Integer -> [Board]
mark bs x = map (\b -> markBoard b x) bs

solve :: Integer -> ([Integer], [Board]) -> (Board, Integer)
solve l ((x:xs), boards)
  | winner >= 0 = (boards !! winner, l)
  | otherwise   = solve x (xs, mark boards x)
  where
    winner = fromIntegral $ getWinner boards 0

parseBoard :: [[String]] -> Board
parseBoard ss = [zip (map read s) (repeat False) | s <- ss]

parseBoards :: [[String]] -> [Board]
parseBoards [] = []
parseBoards ss = parseBoard (take 5 ss) : parseBoards (drop 6 ss)

parse :: [[String]] -> ([Integer], [Board])
parse (s:_:ss) = (map read s, parseBoards ss)

main :: IO()
main = interact $
  show
  .score
  .solve 0
  .parse
  .map words
  .lines