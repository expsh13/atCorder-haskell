module P03 where

main :: IO ()
main = do
  [h, a] <- map (read :: String -> Int) . words <$> getLine
  let count = (h + a - 1) `div` a
  print count
