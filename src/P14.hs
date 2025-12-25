module P14 where

import Data.Char (isUpper, toLower)
import Data.List (sortOn)

main :: IO ()
main = do
  s <- getLine
  let result = concat $ sortOn (map toLower) $ splitStr s []
  putStrLn result

-- 単語に分割
splitStr :: String -> [String] -> [String]
splitStr "" array = array
splitStr (x : xs) array = splitStr remain $ word : array
  where
    (upperBefore, upperAfter) = break isUpper xs
    (word, remain) =
      if null upperAfter
        then (x : upperBefore, "")
        else (x : upperBefore ++ [head upperAfter], tail upperAfter)
