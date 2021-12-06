type Point = (Integer, Integer)
type Line = (Point, Point)
type Board = [[(Bool, Bool)]]

parsePoint :: String -> Point
parsePoint ss = (read p1, read p2)
  where
    p1 = takeWhile (\c -> c >= '0' && c <= '9') ss
    cutS = tail $ dropWhile (\c -> c >= '0' && c <= '9') ss
    p2 = takeWhile (\c -> c >= '0' && c <= '9') cutS

parseLine :: [String] -> Line
parseLine [p1, _, p2] = (parsePoint p1, parsePoint p2)

createBoard :: Int -> Int -> Board
createBoard x y = (take (y+1) . repeat . take (x+1) . repeat) (False, False)

-- End of Parsing

maxX :: [Line] -> Integer
maxX ls = (foldr max 0 . concat . map (\(p1, p2) -> [fst p1, fst p2])) ls

maxY :: [Line] -> Integer
maxY ls = (foldr max 0 . concat . map (\(p1, p2) -> [snd p1, snd p2])) ls

-- End of Helpers

increment :: Point -> Board -> Board
increment (x, y) b
  | fst rx && snd rx = b
  | fst rx = by ++ [bx ++ [(True, True)] ++ ax] ++ ay
  | otherwise = by ++ [bx ++ [(True, False)] ++ ax] ++ ay
  where
    by = take (fromIntegral y) b
    ay = drop (fromIntegral y+1) b
    ry = b !! (fromIntegral y)
    bx = take (fromIntegral x) ry
    ax = drop (fromIntegral x+1) ry
    rx = ry !! (fromIntegral x)

drawLine :: Line -> Board -> Board
drawLine ((x1, y1), (x2, y2)) b
  | x1 /= x2 && y1 /= y2 && (y2 - y1) * (x2 - x1) > 0 = foldr increment b (zip [(min x1 x2)..(max x1 x2)] [(min y1 y2)..(max y1 y2)])
  | x1 /= x2 && y1 /= y2 = foldr increment b (zip [(min x1 x2)..(max x1 x2)] (reverse [(min y1 y2)..(max y1 y2)]))
  | x1 /= x2 = foldr increment b (zip [(min x1 x2)..(max x1 x2)] (repeat y1))
  | y1 /= y2 = foldr increment b (zip (repeat x1) [(min y1 y2)..(max y1 y2)])

drawLines :: Board -> [Line] -> Board
drawLines b ls = foldr drawLine b ls

countDanger :: Board -> Integer
countDanger b = (fromIntegral . sum . map length . map (filter (\s -> fst s && snd s))) b

main :: IO()
main = interact
  $ show
  .countDanger
  .(\ls -> drawLines (createBoard (fromIntegral $ maxX ls) (fromIntegral $ maxY ls)) ls)
  .map parseLine
  .map words
  .lines