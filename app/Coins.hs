import Data.List

data Coin
  = Red
  | Corroded
  | Shiny
  | Concave
  | Blue
  deriving (Show)

coinVal :: Coin -> Int
coinVal Red = 2
coinVal Corroded = 3
coinVal Shiny = 5
coinVal Concave = 7
coinVal Blue = 9

score :: [Int] -> Int
score [a, b, c, d, e] = a + b * c ^ 2 + d ^ 3 - e
score _ = 0

main :: IO ()
main =
  do
    let coins = [Red, Corroded, Shiny, Concave, Blue]
    print $ filter ((== 399) . score . map coinVal) (permutations coins)
