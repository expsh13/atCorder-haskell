module P07 where

import Data.List (sort)

main :: IO ()
main = do
  _ <- getLine
  array <- map (read :: String -> Int) . words <$> getLine
  let sorted = reverse . sort $ array
  let (a, b) = splitNum sorted [] []
  let result = diff a b
  print result

splitNum :: [Int] -> [Int] -> [Int] -> ([Int], [Int])
splitNum [] a b = (a, b)
splitNum (x : xs) a b =
  if length a == length b
    then splitNum xs (x : a) b
    else splitNum xs a (x : b)

diff :: [Int] -> [Int] -> Int
diff a b = alice - bob
  where
    alice = sum a
    bob = sum b