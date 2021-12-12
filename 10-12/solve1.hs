score :: Char -> Integer
score ' ' = 0
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137

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

main :: IO()
main = interact
  $ (\x -> x ++ "\n")
  . show
  . sum
  . map (\(c, xs, ys) -> score c)
  . map (parse [])
  . lines