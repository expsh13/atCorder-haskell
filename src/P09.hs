module P09 where

main :: IO ()
main = do
  a <- readLn :: IO Int -- 500円玉の枚数
  b <- readLn :: IO Int -- 100円玉の枚数
  c <- readLn :: IO Int -- 50円玉の枚数
  x <- readLn :: IO Int -- 目標金額
  let count =
        length
          [ ()
            | i <- [0 .. a],
              j <- [0 .. b],
              k <- [0 .. c],
              500 * i + 100 * j + 50 * k == x
          ]

  print count