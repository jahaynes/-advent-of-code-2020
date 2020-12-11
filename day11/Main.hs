import           Data.Vector      (Vector, (!))
import qualified Data.Vector as V

data Coord =
    Coord !Int !Int deriving Show

_sampleInput1 :: String
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

_sampleInput2 :: String
_sampleInput2 = ".......#.\n\
                \...#.....\n\
                \.#.......\n\
                \.........\n\
                \..#L....#\n\
                \....#....\n\
                \.........\n\
                \#........\n\
                \...#.....\n"

data Grid a =
    Grid { getHeight :: !Int
         , getWidth  :: !Int
         , getCells  :: !(Vector (Vector a))
         } deriving Eq

update :: (Coord -> a -> a) -> Grid a -> Grid a
update f grid =
    grid { getCells =
        V.imap (\x row ->
            V.imap (\y cell ->
                f (Coord x y) cell) row) (getCells grid) }

count :: (a -> Bool) -> Grid a -> Int
count f = V.sum
        . V.map (V.length . V.filter id . V.map f)
        . getCells

plus :: Coord -> (Int, Int) -> Coord
plus (Coord x y) (x', y') = Coord (x+x') (y+y')

inBounds :: Grid a -> Coord -> Bool
inBounds grid (Coord x y) | x  <             0  = False
                          | x >= getHeight grid = False
                          | y  <             0  = False
                          | y >= getWidth grid  = False
                          | otherwise           = True

directions :: [(Int, Int)]
directions = [ (i, j) | i <- [-1, 0, 1],
                        j <- [-1, 0, 1],
                        i /= 0 || j /= 0 ]

lkup :: Grid a -> Coord -> a
lkup grid (Coord x y) = getCells grid ! x ! y

part1 :: Grid Char -> Int
part1 grid =
    let grid' = update step grid
    in if grid' == grid
          then count (=='#') grid'
          else part1 grid'

    where
    step :: Coord -> Char -> Char
    step coord cell

        | cell == 'L' && occupiedNeighbours coord == 0 = '#'

        | cell == '#' && occupiedNeighbours coord >= 4 = 'L'

        | otherwise = cell

    occupiedNeighbours :: Coord -> Int
    occupiedNeighbours = length
                       . filter (=='#')
                       . map (lkup grid)
                       . neighbours

    neighbours :: Coord -> [Coord]
    neighbours coord = filter (inBounds grid)
                     . map (plus coord)
                     $ directions

part2 :: Grid Char -> Int
part2 grid =
    let grid' = update step grid
    in
    if grid' == grid
        then count (=='#') grid'
        else part2 grid'

    where
    step :: Coord -> Char -> Char
    step coord cell

        | cell == 'L' && (length . filter (=='#') $ visible coord) == 0 = '#'

        | cell == '#' && (length . filter (=='#') $ visible coord) >= 5 = 'L'

        | otherwise = cell

    visible :: Coord -> [Char]
    visible coord = castTo coord <$> directions
        where
        castTo c dir =
            let c' = plus c dir
            in
            if inBounds grid c'
                then case lkup grid c' of
                        '.' -> castTo c' dir
                        x   -> x
                else '.'

main :: IO ()
main = do

    input <- readFile "day11/input"

    let grid = parseGrid input

    print $ part1 grid

    print $ part2 grid

    where
    parseGrid :: String -> Grid Char
    parseGrid input =
        let cells = V.fromList (V.fromList <$> lines input)
        in Grid { getHeight = length cells
                , getWidth  = length $ V.head cells
                , getCells  = cells
                }