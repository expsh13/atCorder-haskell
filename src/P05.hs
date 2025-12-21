module P05 where

main :: IO ()
main = do
  [n, a, b] <- map (read :: String -> Int) . words <$> getLine
  -- a,bは36以下の整数。

  -- 1. 格桁のMaxの数は上記なのでaの最少の値を確認し、何桁から確認する必要があるか決める。
  -- 2. 1で決まった値からNまでの値で制約を満たす整数を調べる
  let minNum = (10 :: Int) ^ (minDigit a - 1)
  let result = passedSum a b n minNum 0
  print result

-- 1桁→9
-- 2桁→18
-- 3桁→27
-- 4桁→36
minDigit :: Int -> Int
minDigit a
  | a > 27 = 4
  | a > 18 = 3
  | a > 9 = 2
  | otherwise = 1

-- 格桁の総和
digitSum :: Int -> Int
digitSum n = sum $ map (read . return) (show n)

passedSum :: Int -> Int -> Int -> Int -> Int -> Int
passedSum a b n curNum total
  | curNum > n = total
  | digitSum curNum >= a && digitSum curNum <= b =
      passedSum a b n (curNum + 1) (total + curNum)
  | otherwise = passedSum a b n (curNum + 1) total
