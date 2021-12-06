type School = [Integer]

pass_day :: School -> School
pass_day s = (concat . map procreate . map (\f -> f - 1)) s
  where
    procreate :: Integer -> School
    procreate f
      | f >= 0 = [f]
      | otherwise = [6, 8]

main :: IO()
main = interact
  $(\x -> x ++ "\n")
  .show
  .length
  .(\s -> foldr (\_ school -> pass_day school) s [1..80])
  .map read
  .words