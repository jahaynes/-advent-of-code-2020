import           Data.Vector.Unboxed      ((!))
import qualified Data.Vector.Unboxed as V

_sampleInput :: String
_sampleInput = "35\n\
               \20\n\
               \15\n\
               \25\n\
               \47\n\
               \40\n\
               \62\n\
               \55\n\
               \65\n\
               \95\n\
               \102\n\
               \117\n\
               \150\n\
               \182\n\
               \127\n\
               \219\n\
               \299\n\
               \277\n\
               \309\n\
               \576"

bufSize :: Int
bufSize = 25

part1 :: [Int] -> Maybe Int
part1 = go V.empty
    where
    go    _     [] = Nothing
    go wind (x:xs) | V.length wind < bufSize = go (wind `V.snoc` x) xs
                   | otherwise =
        let sums = [wind ! i + wind ! j | i <- [  0..bufSize-1]
                                        , j <- [i+1..bufSize-1] ]
        in
        if any (==x) sums
            then go (V.tail wind `V.snoc` x) xs
            else Just x

part2 :: Int -> [Int] -> Int
part2 p1 = go . V.fromList
    where
    go xs =
        let sums = do i <- [  0..V.length xs-1]
                      j <- [i+1..V.length xs]
                      let range = V.slice i (j-i) xs
                      let s = V.sum range
                      if s == p1
                          then [(V.minimum range + V.maximum range)]
                          else []
        in head sums

main :: IO ()
main = do

    input <- map read . lines <$>  readFile "day09/input"

    let Just p1 = part1 input
    print p1

    print $ part2 p1 input