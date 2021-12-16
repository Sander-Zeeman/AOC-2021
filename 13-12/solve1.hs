parsePoint :: String -> (Int, Int)
parsePoint xs = (read $ tail $ dropWhile cond xs, read $ takeWhile cond xs)
  where cond x = fromEnum x >= fromEnum '0' && fromEnum x <= fromEnum '9'

parseTask :: [String] -> (Int, Bool)
parseTask parts = parsePart (last parts)
  where parsePart (dim : '=' : rest) = (read rest, dim == 'y')

-----------------------------------------------------------------------------------

createRow :: [(Int, Int)] -> Int -> [Bool]
createRow pos i = map (\x -> elem x filtered) [0..(foldr max 0 (map snd pos))]
  where
    filtered = map snd $ filter (\x -> fst x == i) pos

createBoard :: [(Int, Int)] -> [[Bool]]
createBoard pos = [createRow pos i | i <- [0..(foldr max 0 (map fst pos))]]

-----------------------------------------------------------------------------------

bigOr :: [[Bool]] -> [[Bool]] -> [[Bool]]
bigOr = zipWith (zipWith (||))

fold :: [[Bool]] -> (Int, Bool) -> [[Bool]]
fold board (x, False) = bigOr (map (take x) board) (map reverse $ map (drop (x+1)) board)
fold board (y, True) = bigOr (take y board) (reverse $ drop (y+1) board)

-----------------------------------------------------------------------------------

main :: IO()
main = interact
  $ (\x -> x ++ "\n")
  . show
  . sum
  . map (fromIntegral . length)
  . map (filter (&& True))
  . (\(board, tasks) -> fold board (head tasks))
  . (\(points, tasks) -> (createBoard points, tasks))
  . (\(pos, tasks) -> (map parsePoint pos, map parseTask tasks))
  . (\xs -> (takeWhile (/= "") xs, map words $ tail $ dropWhile (/= "") xs))  
  . lines