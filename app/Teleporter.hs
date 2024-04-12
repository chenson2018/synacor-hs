-- thanks to https://gist.github.com/LeoAdamek/b00ecd9380007ee2a666
-- trying to change this to be a function of x dratically reduces performance
import Data.List

teleport :: Int -> [[Int]]
teleport x = aux
  where
    aux :: [[Int]]
    aux =
      [ [ let m_dec = (m - 1) `mod` 32768
           in case (m, n) of
                (0, _) -> (n + 1) `mod` 32768
                (_, 0) -> aux !! m_dec !! x
                (_, _) -> aux !! m_dec !! (aux !! m !! ((n - 1) `mod` 32768))
          | n <- [0 ..]
        ]
        | m <- [0 ..]
      ]

-- takes about ~10 seconds per register attempt, so just putting the answer in
main :: IO ()
main = print $ find (\reg -> 6 == teleport reg !! 4 !! 1) [25734]
