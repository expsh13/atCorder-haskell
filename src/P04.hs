module P04 where

data Result = OK | NG deriving (Show)

main :: IO ()
main = do
  k <- read <$> getLine
  [a, b] <- map (read :: String -> Int) . words <$> getLine

  let firstMultiple = ((a + k - 1) `div` k) * k
  let result = if firstMultiple <= b then OK else NG
  print result

-- O(B-A) になる。
-- checkKTimes :: (Int, Int, Int) -> Result
-- checkKTimes (k, a, b)
--   | b `mod` k == 0 = OK
--   | nextB < a = NG
--   | otherwise = checkKTimes (k, a, nextB)
--   where
--     nextB = b - 1