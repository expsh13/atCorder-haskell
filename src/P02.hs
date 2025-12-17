module P02 where

main :: IO ()
main = do
  [a, b] <- map (read :: String -> Int) . words <$> getLine
  let multi = a * b
  if even multi
    then putStrLn "Even"
    else putStrLn "Odd"
