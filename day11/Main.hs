{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Vector.Unboxed         ((!))
import qualified Data.Vector.Unboxed as VU
import           Data.Word                   (Word8)

data Coord =
    Coord !Int !Int deriving Show

_sampleInput1 :: ByteString
_sampleInput1 = "L.LL.LL.LL\n\
                \LLLLLLL.LL\n\
                \L.L.L..L..\n\
                \LLLL.LL.LL\n\
                \L.LL.LL.LL\n\
                \L.LLLLL.LL\n\
                \..L.L.....\n\
                \LLLLLLLLLL\n\
                \L.LLLLLL.L\n\
                \L.LLLLL.LL\n"

_sampleInput2 :: ByteString
_sampleInput2 = ".......#.\n\
                \...#.....\n\
                \.#.......\n\
                \.........\n\
                \..#L....#\n\
                \....#....\n\
                \.........\n\
                \#........\n\
                \...#.....\n"

data Grid =
    Grid { getHeight :: !Int
         , getWidth  :: !Int
         , getCells  :: !(VU.Vector Word8)
         } deriving Eq

update :: (Coord -> Word8 -> Word8) -> Grid -> Grid
update f grid =
    grid { getCells =
        VU.imap (\i cell -> let (x, y) = i `divMod` getWidth grid
                            in f (Coord x y) cell) (getCells grid) }

countHashes :: Grid -> Int
countHashes = VU.length 
            . VU.filter (==hash)
            . getCells

plus :: Coord -> (Int, Int) -> Coord
plus (Coord x y) (x', y') = Coord (x+x') (y+y')

inBounds :: Grid -> Coord -> Bool
inBounds grid (Coord x y) | x  <             0  = False
                          | x >= getHeight grid = False
                          | y  <             0  = False
                          | y >= getWidth grid  = False
                          | otherwise           = True

directions :: [(Int, Int)]
directions = [ (i, j) | i <- [-1, 0, 1],
                        j <- [-1, 0, 1],
                        i /= 0 || j /= 0 ]

lkup :: Grid -> Coord -> Word8
lkup grid (Coord x y) = getCells grid ! (x * getWidth grid + y)

hash, el, dot :: Word8
hash = 35
el   = 76
dot  = 46

part1 :: Grid -> Int
part1 grid =
    let grid' = update step grid
    in if grid' == grid
          then countHashes grid'
          else part1 grid'

    where
    step :: Coord -> Word8 -> Word8
    step coord cell

        | cell == el   && occupiedNeighbours coord == 0 = hash

        | cell == hash && occupiedNeighbours coord >= 4 = el

        | otherwise = cell

    occupiedNeighbours :: Coord -> Int
    occupiedNeighbours = length
                       . filter (==hash)
                       . map (lkup grid)
                       . neighbours

    neighbours :: Coord -> [Coord]
    neighbours coord = filter (inBounds grid)
                     . map (plus coord)
                     $ directions

part2 :: Grid -> Int
part2 grid =
    let grid' = update step grid
    in
    if grid' == grid
        then countHashes grid'
        else part2 grid'

    where
    step :: Coord -> Word8 -> Word8
    step coord cell

        | cell == el   && (length . filter (==hash) $ visible coord) == 0 = hash

        | cell == hash && (length . filter (==hash) $ visible coord) >= 5 = el

        | otherwise = cell

    visible :: Coord -> [Word8]
    visible coord = castTo coord <$> directions
        where
        castTo c dir =
            let c' = plus c dir
            in
            if inBounds grid c'
                then let x = lkup grid c'
                     in if x == dot
                            then castTo c' dir
                            else x
                else dot

main :: IO ()
main = do

    input <- C8.readFile "day11/input"

    let grid = parseGrid input

    print $ part1 grid

    print $ part2 grid

    where
    parseGrid :: ByteString -> Grid
    parseGrid input =
        let len    = C8.length input
            height = length $ C8.lines input
            width  = len `div` height
            cells  = VU.fromListN len $ BS.unpack input
        in Grid { getHeight = height
                , getWidth  = width
                , getCells  = cells
                }
