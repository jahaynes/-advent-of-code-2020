import Data.List (sort)

import           Data.IntMap.Strict      (IntMap, (!))
import qualified Data.IntMap.Strict as M

_sampleInput1 :: String
_sampleInput1 = "16\n\
                \10\n\
                \15\n\
                \5\n\
                \1\n\
                \11\n\
                \7\n\
                \19\n\
                \6\n\
                \12\n\
                \4"

part1 :: [Int] -> Int
part1     [] = error "No input"
part1 (a:as) =
    let diffs = go a mempty as
    in diffs ! 1 * diffs ! 3

    where
    go    _ diffs     [] = diffs
    go jolt diffs (x:xs) =
        let diff = x - jolt
        in
        if diff < 1 || diff > 3
            then error $ "diff: " ++ show (jolt, x)
            else go x (M.alter f diff diffs) xs

        where
        f (Just d) = Just $! d + 1
        f  Nothing = Just 1

part2 :: [Int] -> IntMap Int
part2       [] = error "no input"
part2      [n] = M.singleton n 1
part2 (n:outs) =

    let degreeMap = part2 outs

        outDegrees = map (degreeMap !)
                   . takeWhile (\o -> o - n <= 3)
                   $ outs

    in M.insert n (sum outDegrees) degreeMap

main :: IO ()
main = do

    input <- map read . lines <$> readFile "day10/input"

    let sorted  = sort (0:input)
        adaptor = 3 + last sorted
        chain   = sorted ++ [adaptor]

    print $ part1 chain

    print $ part2 chain ! head chain
