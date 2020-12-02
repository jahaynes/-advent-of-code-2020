import Common.Parser
import Common.Parser.String
import Common.Parser.Combinator

import Data.List (sort)

newtype Input =
    Input [Int]
        deriving Show

parseInput :: Parser String Input
parseInput = Input <$> many (int <* endOfLine)

_sampleInput :: Input
_sampleInput = Input [1721, 979, 366, 299, 675, 1456]

-- Chop any inputs too big to sum to 2020 with the smallest element
cull2 :: Input -> Input 
cull2 (Input xs) | any (< 0) xs = Input xs
                 | otherwise =
                     let smallest = minimum xs
                         rest     = filter (\x -> x + smallest <= 2020) xs
                     in Input rest

-- Chop any inputs too big to sum to 2020 with the smallest two elements
cull3 :: Input -> Input 
cull3 (Input xs) | any (< 0) xs = Input xs
                 | otherwise =
                     let [smal,lest] = take 2 . sort $ xs
                         rest        = filter (\x -> x + smal + lest <= 2020) xs
                     in Input rest

sum2To2020 :: Input -> [Int]
sum2To2020 (Input xs) = do
    let len = length xs
    (x, i) <- zip xs [1..len]
    (y, j) <- zip xs [1..len]
    if i >= j
        then []
        else
            if x + y == 2020
                then [x, y]
                else []

sum3To2020 :: Input -> [Int]
sum3To2020 (Input xs) = do
    let len = length xs
    (x, i) <- zip xs [1..len]
    (y, j) <- zip xs [1..len]
    if i >= j
        then []
        else do
            (z, k) <- zip xs [1..len]
            if j >= k
                then []
                else
                    if x + y + z == 2020
                        then [x, y, z]
                        else []

main :: IO ()
main = do

    Right input <- parse parseInput <$> readFile "day01/input"

    part1 input

    part2 input

    where
    part1 = print . product . sum2To2020 . cull2

    part2 = print . product . sum3To2020 . cull3
