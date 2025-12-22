{-# OPTIONS_GHC -Wno-name-shadowing #-}

module P06 where

main :: IO ()
main = do
  n <- getLine
  putStrLn "配列入力"
  array <- map (read :: String -> Int) . words <$> getLine
  let result = foldl min 19683 (map evenCount array)
  print result

evenCount :: Int -> Int
evenCount n = evenCountHelper n 0
  where
    evenCountHelper n c =
      if even n
        then evenCountHelper (n `div` 2) (c + 1)
        else c

logBase2 :: Double -> Int
logBase2 n = floor log2
  where
    log2 = logBase 2 n