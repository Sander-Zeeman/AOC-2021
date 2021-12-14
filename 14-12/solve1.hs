parseRule :: String -> (String, Char)
parseRule rule = (head rules, head $ last rules)
  where rules = words rule

applyRules :: [(String, Char)] -> Char -> Char -> Char
applyRules [] _ _ = ' '
applyRules ((b, a):rs) x y
  | [x,y] == b = a
  | otherwise = applyRules rs x y

alternate :: String -> String -> Bool -> String
alternate (as) [] _ = (as)
alternate [] bs _ = bs
alternate (a:as) (b:bs) side
  | side = a : alternate (as) (b:bs) False
  | b == ' ' = alternate (a:as) bs True
  | otherwise = b : alternate (a:as) bs True

apply :: [(String, Char)] -> String -> String
apply rules string = alternate string adds True
  where adds = zipWith (applyRules rules) (init string) (tail string)

countElements :: [Integer] -> String -> [Integer]
countElements current [] = current
countElements current (s:ss) = countElements (before ++ mid  ++ end) ss
  where
    before = take (fromEnum s - fromEnum 'A') current
    mid = [1 + (head $ drop (fromEnum s - fromEnum 'A') current)]
    end = drop (1 + fromEnum s - fromEnum 'A') current

main :: IO()
main = interact
  $ (\x -> x ++ "\n")
  . show
  . (\xs -> (foldr max 0 xs) - (foldr min 999999999 xs))
  . filter (>0)
  . countElements (take 26 $ repeat 0)
  . head
  . drop 10
  . (\(s, rules) -> iterate (apply rules) s)
  . (\(x:xs) -> (x, map parseRule $ tail xs))
  . lines