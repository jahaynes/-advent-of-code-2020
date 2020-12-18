
import Common.Parser
import Common.Parser.Combinator
import Common.Parser.String

import Data.Char (isSpace)
import Data.List (foldl')

import Prelude hiding (dropWhile)

_sampleInput :: String
_sampleInput = "F10\n\
               \N3\n\
               \F7\n\
               \R90\n\
               \F11"

part1 :: [(Char, Int)] -> Int
part1 instrs = let (_, (x, y)) = foldl' step (90, (0, 0)) instrs
               in abs x + abs y

    where
    step (bearing, c@(x,y)) i =

        let bearing' =
                case i of
                    ('L', a) -> (bearing - a) `mod` 360
                    ('R', a) -> (bearing + a) `mod` 360
                    _        -> bearing

            coord' =
                case i of
                    ('N', a) -> (x    , y + a)
                    ('S', a) -> (x    , y - a)
                    ('E', a) -> (x + a, y    )
                    ('W', a) -> (x - a, y    )
                    ('F', a) -> case bearing' of
                                    0   -> (x    , y + a)
                                    90  -> (x + a, y    )
                                    180 -> (x    , y - a)
                                    270 -> (x - a, y    )
                    _        -> c

        in (bearing', coord')

part2 :: [(Char, Int)] -> Int
part2 instrs = let (_, (x, y)) = foldl' step ((10, 1), (0, 0)) instrs
               in abs x + abs y
    where
    step (wp@(wx,wy), ship@(sx,sy)) i =

        let ship' = case i of
                        ('F', a) -> (sx + a * wx, sy + a * wy)
                        _        -> ship

            wp' = case i of
                      ('N', a) -> (wx    , wy + a)
                      ('S', a) -> (wx    , wy - a)
                      ('E', a) -> (wx + a, wy    )
                      ('W', a) -> (wx - a, wy    )
                      ('R', a) -> rot a
                      ('L', a) -> rot ((-a) `mod` 360)
                      ('F', _) -> wp

        in (wp', ship')

        where
        rot  90 = ( wy, -wx)
        rot 180 = (-wx, -wy)
        rot 270 = (-wy,  wx)

main :: IO ()
main = do

    Right input <- parse parser <$> readFile "day12/input"

    print $ part1 input

    print $ part2 input

    where
    parser :: Parser String [(Char, Int)]
    parser = dropWhile isSpace *> many (instr <* dropWhile isSpace)
        where
        instr = (,) <$> one <*> int
