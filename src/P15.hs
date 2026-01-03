module P15 where

import Control.Monad (replicateM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  [n, m, q] <- getInts
  pair <- replicateM m getInts
  c <- getInts
  let pairMap = initialPairMap n pair
  let colorMap = initialColorMap c
  s <- replicateM q getInts
  mapM_ print $ loopQuery s colorMap pairMap []

getInts :: IO [Int]
getInts = map (read :: String -> Int) . words <$> getLine

initialColorMap :: [Int] -> Map Int Int
initialColorMap colors = Map.fromList $ zip [1 ..] colors

initialPairMap :: Int -> [[Int]] -> Map Int [Int]
initialPairMap n = foldr addEdge initialMap
  where
    -- 全頂点を空リストで初期化
    initialMap = Map.fromList [(i, []) | i <- [1 .. n]]
    -- 各辺を双方向に追加
    addEdge [u, v] m = Map.insertWith (++) u [v] $ Map.insertWith (++) v [u] m
    addEdge _ m = m -- 不正な入力を無視

loopQuery :: [[Int]] -> Map Int Int -> Map Int [Int] -> [Int] -> [Int]
loopQuery [] _ _ result = result
loopQuery (x : xs) cMap pMap array = loopQuery xs newCMap pMap $ array ++ [num]
  where
    (num, newCMap) = runQuery x cMap pMap

runQuery :: [Int] -> Map Int Int -> Map Int [Int] -> (Int, Map Int Int)
runQuery (1 : x : _) cMap pMap = (currentColor, newColorMap)
  where
    currentColor = cMap Map.! x
    neighbors = Map.findWithDefault [] x pMap
    -- 隣接頂点の色を currentColor に更新
    newColorMap = foldr (\v m -> Map.insert v currentColor m) cMap neighbors
runQuery (2 : x : y : _) cMap _ = (currentColor, newColorMap)
  where
    currentColor = cMap Map.! x
    newColorMap = Map.insert x y cMap
-- 到達しないはずなのでerror
runQuery _ _ _ = error "Invalid query"