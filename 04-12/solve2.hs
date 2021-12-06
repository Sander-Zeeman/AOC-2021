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

markBoard :: Board -> Integer -> Board
markBoard b x = map (map (\(i, s) -> (i, s || i == x))) b

mark :: [Board] -> Integer -> [Board]
mark bs x = map (\b -> markBoard b x) bs

solve :: ([Integer], [Board]) -> (Board, [Integer])
solve (gs, (b:[])) = (b, gs)
solve ((g:gs), (b:bs)) = solve (gs, filter (\b -> not (vertical b || horizontal b)) (mark (b:bs) g))

findWinning :: (Board, [Integer]) -> (Board, Integer)
findWinning (b, (g:gs))
  | horizontal marked || vertical marked = (marked, g)
  | otherwise = findWinning (marked, gs)
  where marked = markBoard b g

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
  .findWinning
  .solve
  .parse
  .map words
  .lines