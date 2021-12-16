parsePoint :: String -> (Int, Int)
parsePoint xs = (read $ tail $ dropWhile cond xs, read $ takeWhile cond xs)
  where cond x = fromEnum x >= fromEnum '0' && fromEnum x <= fromEnum '9'

parseTask :: [String] -> (Int, Bool)
parseTask parts = parsePart (last parts)
  where parsePart (dim : '=' : rest) = (read rest, dim == 'y')

-----------------------------------------------------------------------------------

createRow :: [(Int, Int)] -> Int -> Int -> [Bool]
createRow pos i maxX = map (\x -> elem x filtered) [0..maxX]
  where
    filtered = map snd $ filter (\x -> fst x == i) pos

createBoard :: [(Int, Int)] -> [(Int, Bool)] -> [[Bool]]
createBoard pos tasks = [createRow pos i maxX | i <- [0..maxY]]
  where
    maxX = 2 * foldr (\(d,b) i -> if b then i else (max d i)) 0 tasks
    maxY = 2 * foldr (\(d,b) i -> if b then (max d i) else i) 0 tasks

-----------------------------------------------------------------------------------

bigOr :: [[Bool]] -> [[Bool]] -> [[Bool]]
bigOr = zipWith (zipWith (||))

fold :: [[Bool]] -> (Int, Bool) -> [[Bool]]
fold board (x, False) = bigOr (map (take x) board) (map reverse $ map (drop (x+1)) board)
fold board (y, True) = bigOr (take y board) (reverse $ drop (y+1) board)

-----------------------------------------------------------------------------------

transform :: [[Bool]] -> [String]
transform = map (map (\c -> if c then '#' else ' '))

-----------------------------------------------------------------------------------

main :: IO()
main = interact
  $ unlines
  . transform
  . (\(board, tasks) -> foldl fold board tasks)
  . (\(points, tasks) -> (createBoard points tasks, tasks))
  . (\(pos, tasks) -> (map parsePoint pos, map parseTask tasks))
  . (\xs -> (takeWhile (/= "") xs, map words $ tail $ dropWhile (/= "") xs))  
  . lines