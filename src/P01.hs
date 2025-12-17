module P01 where

main :: IO ()
main = do
  [a, b] <- map (read :: String -> Int) . words <$> getLine
  print (a * b)
