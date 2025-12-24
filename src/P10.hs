module P10 where

import Control.Monad (replicateM)
import Data.Char (intToDigit)

main :: IO ()
main = do
  [h, w] <- getInts
  grid <- replicateM h getLine
  let result = transformGrid grid h w
  mapM_ putStrLn result

getInts :: IO [Int]
getInts = map (read :: String -> Int) . words <$> getLine

isMine :: [String] -> Int -> Int -> Int -> Int -> Bool
isMine grid h w i j = isRange && (grid !! i !! j) == '#'
  where
    isRange = i >= 0 && i < h && j >= 0 && j < w

-- 周囲8方向の爆弾をカウント
countMines :: [String] -> Int -> Int -> Int -> Int -> Int
countMines grid h w i j = length [() | di <- [-1 .. 1], dj <- [-1 .. 1], (di, dj) /= (0, 0), isMine grid h w (i + di) (j + dj)]

transformCell :: [String] -> Int -> Int -> Int -> Int -> Char
transformCell grid h w i j =
  if isMine grid h w i j
    then '#'
    else intToDigit $ countMines grid h w i j

transformGrid :: [String] -> Int -> Int -> [String]
transformGrid grid h w = [[transformCell grid h w i j | j <- [0 .. w - 1]] | i <- [0 .. h - 1]]