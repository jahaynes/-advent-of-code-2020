import Data.Functor ((<&>))
import Data.List    (sort)

_sampleInput :: String
_sampleInput = "FBFBBFFRLR\n\
               \BFFFBBFRRR\n\
               \FFFBBBFRRR\n\
               \BBFFBBFRLL"

newtype Id = Id Int
    deriving (Eq, Ord, Show)

newtype Plane =
    Plane [[Id]]

plane :: Plane
plane = Plane $ [0..127] <&> \row -> map (\s -> Id $ row * 8 + s) [0..7]

findSeating :: Plane -> String -> Id
findSeating (Plane [[x]])     _  = x
findSeating (Plane [row]) (x:xs) =
    let (lower, upper) = splitAt (length row `div` 2) row
    in case x of
           'L' -> findSeating (Plane [lower]) xs
           'R' -> findSeating (Plane [upper]) xs
findSeating (Plane rows) (x:xs) =
    let (lower, upper) = splitAt (length rows `div` 2) rows
    in case x of
           'F' -> findSeating (Plane lower) xs
           'B' -> findSeating (Plane upper) xs

findGap :: [Id] -> Id
findGap ss = go (sort ss)
    where
    go             [] = error "not found"
    go (Id x:Id y:zs) | x + 1 == y = go (Id y:zs)
                      | x     == y = go (Id y:zs)
                      | otherwise  = Id $ x + 1

main :: IO ()
main = do

    input <- lines <$> readFile "day05/input"

    let seating = map (findSeating plane) input

    part1 seating

    part2 seating

    where
    part1 = print . maximum

    part2 = print . findGap
