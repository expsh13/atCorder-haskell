module P13 where

import Control.Monad (replicateM)
import qualified Data.Map as Map

main :: IO ()
main = do
  n <- (read :: String -> Int) <$> getLine
  s <- replicateM n getLine
  let result = sum $ anagramArray s []
  print result

-- アナグラムの数配列
anagramArray :: [String] -> [Int] -> [Int]
anagramArray [] array = array
anagramArray (x : xs) array = anagramArray xs $ countAnagram x xs 0 : array

-- ある要素と同じアナグラムが何個あるか
countAnagram :: String -> [String] -> Int -> Int
countAnagram _ [] c = c
countAnagram str (x : xs) c =
  if isAnagram str x
    then countAnagram str xs c + 1
    else countAnagram str xs c

-- アナグラムになっているかどうか
isAnagram :: String -> String -> Bool
isAnagram a b = aMap == bMap
  where
    aMap = createMap a Map.empty
    bMap = createMap b Map.empty

createMap :: String -> Map.Map Char Int -> Map.Map Char Int
createMap "" mapItem = mapItem
createMap (x : xs) mapItem = case Map.lookup x mapItem of
  Just value -> createMap xs $ Map.insert x (value + 1) mapItem
  Nothing -> createMap xs $ Map.insert x 1 mapItem
