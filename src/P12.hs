module P12 where

import Control.Monad (replicateM)
import qualified Data.Map as Map

main :: IO ()
main = do
  n <- getInt
  d <- replicateM n getInt
  let result = Map.size $ createMap d Map.empty
  print result

getInt :: IO Int
getInt = (read :: String -> Int) <$> getLine

createMap :: [Int] -> Map.Map Int Int -> Map.Map Int Int
createMap [] mapItem = mapItem
createMap (x : xs) mapItem = case Map.lookup x mapItem of
  Just _ -> createMap xs mapItem
  Nothing -> createMap xs $ Map.insert x 1 mapItem
