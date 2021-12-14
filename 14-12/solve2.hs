parseRule :: String -> (String, Char)
parseRule rule = (head rules, head $ last rules)
  where rules = words rule

set :: Char -> Char -> Integer -> [[Integer]] -> [[Integer]]
set s t b current = (yb ++ [xb ++ [actual + b] ++ xa] ++ ya)
  where
    sInt = fromEnum s - fromEnum 'A'
    tInt = fromEnum t - fromEnum 'A'
    yb = take sInt current
    ya = drop (1 + sInt) current
    xList = current !! sInt
    xb = take tInt xList
    xa = drop (1 + tInt) xList
    actual = xList !! tInt

countPairs :: String -> [[Integer]] -> [[Integer]]
countPairs [] current = current
countPairs (x:[]) current = current
countPairs (s:t:ss) current = countPairs (t:ss) (set s t 1 current)

createBoard :: [[Integer]]
createBoard = take 26 (repeat (take 26 (repeat 0)))

add :: [[Integer]] -> [[Integer]] -> [[Integer]]
add = zipWith (zipWith (+))

step :: [(String, Char)] -> [[Integer]] -> [[Integer]]
step [] counts = counts
step ((before,after):rs) counts = add added2 (step rs counts)
  where
    b0int = fromEnum (head before) - fromEnum 'A'
    b1int = fromEnum (last before) - fromEnum 'A'
    aint = fromEnum after - fromEnum 'A'

    iter = (counts !! b0int) !! b1int
    newZeros = createBoard
    
    subtracted = set (head before) (last before) (-iter) newZeros
    added1 = set (head before) after iter subtracted
    added2 = set after (last before) iter added1

mix :: [[Integer]] -> Char -> [Integer]
mix board l = head $ add [lst] [[sum (board !! i) | i <- [0..25]]]
  where
    c = fromEnum l - fromEnum 'A'
    lst = (take c (repeat 0)) ++ [1] ++ (take (26-c-1) (repeat 0))

main :: IO()
main = interact
  $ (\x -> x ++ "\n")
  . show
  . (\xs -> (foldr max 0 xs) - (foldr min 9999999999999999 xs))
  . filter (>0)
  . (\(x, lst) -> mix x lst)
  . (\(x, lst) -> (head x, lst))
  . (\(x, lst) -> (drop 40 x, lst))
  . (\(counts, rules, lst) -> (iterate (step rules) counts, lst))
  . (\(s, rules) -> (countPairs s createBoard, rules, last s))
  . (\(x:xs) -> (x, map parseRule $ tail xs))
  . lines