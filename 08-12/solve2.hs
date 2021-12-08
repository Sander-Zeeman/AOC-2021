match :: [Integer] -> Integer
match [1,1,1,0,1,1,1] = 0
match [0,0,1,0,0,1,0] = 1
match [1,0,1,1,1,0,1] = 2
match [1,0,1,1,0,1,1] = 3
match [0,1,1,1,0,1,0] = 4
match [1,1,0,1,0,1,1] = 5
match [1,1,0,1,1,1,1] = 6
match [1,0,1,0,0,1,0] = 7
match [1,1,1,1,1,1,1] = 8
match [1,1,1,1,0,1,1] = 9

translate_solo :: String -> String -> Integer
translate_solo key code = match $ map (\c -> if elem c code then 1 else 0) key

translate :: (String, [String]) -> [Integer]
translate (key, encoded) = map (translate_solo key) encoded

count :: [String] -> [(Char, Integer)]
count encoded = zip ['a'..'g'] $ map (\c -> (fromIntegral . length . filter (elem c)) encoded) ['a'..'g']

find :: [(Char, Integer)] -> Integer -> String
find counts x = (map fst . filter (\(c, i) -> i == x)) counts

decideSevens :: [String] -> String -> (Char, Char)
decideSevens encoded [op1, op2]
  | elem op1 four = (op1, op2)
  | otherwise     = (op2, op1)
  where four = head $ filter (\s -> length s == 4) encoded

decideEights :: [String] -> String -> (Char, Char)
decideEights encoded [op1, op2]
  | elem op1 one = (op2, op1)
  | otherwise    = (op1, op2)
  where one = head $ filter (\s -> length s == 2) encoded

decode :: [String] -> String
decode encoded = [a,b,c,d,e,f,g]
  where
    counts = count encoded
    sevens = find counts 7
    eights = find counts 8
    (a, c) = decideEights encoded eights
    b = head $ find counts 6
    (d, g) = decideSevens encoded sevens
    e = head $ find counts 4
    f = head $ find counts 9

main :: IO()
main = interact
  $(\x -> x ++ "\n")
  .show
  .sum
  .map (read . concat . map show)
  .map translate
  .map (\(a, b) -> (decode a, b))
  .map (\xs -> (takeWhile ("|" /=) xs, tail $ dropWhile ("|" /=) xs))
  .map words
  .lines