module P08 where

import Data.Char (isLower)

data Result = AC | WA deriving (Show)

main :: IO ()
main = do
  s <- getLine
  let result = check s
  print result

isStartA :: String -> Bool
isStartA str = head str == 'A'

isInCludeC :: String -> Bool
isInCludeC str = (== 1) $ length $ filter (== 'C') filterStr
  where
    filterStr = init $ tail $ tail str

isLowerCase :: String -> Bool
isLowerCase str = all isLower nonACChars
  where
    tailStr = tail str -- 先頭のA以外
    withoutLast = init tailStr -- 2文字目〜末尾-1文字目
    lastCh = last tailStr -- 末尾
    nonACChars = filter (/= 'C') withoutLast ++ [lastCh]

check :: String -> Result
check str =
  if isStartA str && isInCludeC str && isLowerCase str
    then AC
    else WA