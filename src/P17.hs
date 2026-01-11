module P17 where

import Control.Monad (guard)
import GHC.Natural (Natural)

main :: IO ()
main = do
  putStrLn "P17"

divisorsOfOdds :: [Natural] -> [Natural]
divisorsOfOdds ints = do
  i <- ints
  guard $ i `rem` 2 == 1
  j <- [1 .. i]
  guard $ i `rem` j == 0
  pure j