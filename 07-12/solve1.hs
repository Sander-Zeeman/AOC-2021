minList :: [Integer] -> Integer
minList = foldr min (10^1000)

maxList :: [Integer] -> Integer
maxList = foldr max 0

findDistances :: [Integer] -> Integer -> Integer
findDistances xs i = (sum . map abs . map (i-)) xs

main :: IO()
main = interact
  $ (\xs -> xs ++ "\n")
  . show
  . minList
  . (\xs -> map (findDistances xs) [minList xs..maxList xs])
  . map read
  . words