module P16 where

import Control.Monad (replicateM)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  [_, q] <- getInts
  s <- getLine
  querys <- replicateM q getInts
  let result = unlines $ map show $ loopQuery s querys
  putStrLn result

getInts :: IO [Int]
getInts = map (read :: String -> Int) . words <$> getLine

countAC :: String -> Int
countAC str = length (splitOn "AC" str) - 1

loopQuery :: String -> [[Int]] -> [Int]
loopQuery str = map (runQuery str)

runQuery :: String -> [Int] -> Int
runQuery str query = countAC s
  where
    l = head query
    r = head $ tail query
    s = drop (l - 1) (take r str)