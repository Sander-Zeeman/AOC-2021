score :: Char -> Integer
score ')' = 1
score ']' = 2
score '}' = 3
score '>' = 4

match :: Char -> Char
match '(' = ')'
match '[' = ']'
match '{' = '}'
match '<' = '>'

parse :: String -> String -> (Char, String, String)
parse opened [] = (' ', opened, "")
parse [] (r:remaining)
  | elem r "([{<" = parse [r] remaining
  | otherwise = (r, "", remaining)
parse (o:opened) (r:remaining)
  | elem r "([{<" = parse (r:o:opened) remaining
  | match o == r = parse opened remaining
  | otherwise = (r, (o:opened), remaining)

insert :: Integer -> [Integer] -> [Integer]
insert x [] = [x]
insert x (y:ys)
  | x < y = x : y : ys
  | otherwise = y : (insert x ys)

sort :: [Integer] -> [Integer]
sort [] = []
sort (x:xs) = insert x (sort xs)

main :: IO()
main = interact
  $ (\x -> x ++ "\n")
  . show
  . (\xs -> xs !! (div (length xs) 2))
  . sort
  . map (foldr (\a b -> b * 5 + score a) 0)
  . map reverse
  . map (map match)
  . map (\(c, xs, ys) -> xs)
  . filter (\(c, _, _) -> c == ' ') 
  . map (parse [])
  . lines