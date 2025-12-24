module P11 where

main :: IO ()
main = do
  [n, y] <- map (read :: String -> Int) . words <$> getLine
  let (a, b, c) = checkPattern n y
  putStrLn $ show a ++ " " ++ show b ++ " " ++ show c

checkPattern :: Int -> Int -> (Int, Int, Int)
checkPattern n y =
  if null pattern
    then (-1, -1, -1)
    else head pattern
  where
    pattern = [(a, b, c) | a <- [0 .. n], b <- [0 .. n - a], let c = n - a - b, c >= 0, y == 10000 * a + 5000 * b + 1000 * c]
