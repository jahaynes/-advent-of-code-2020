import Common.Parser
import Common.Parser.Combinator
import Common.Parser.String

import           Data.Char        (isSpace)
import           Data.List        (group, sort)
import           Data.Vector      (Vector, (!))
import qualified Data.Vector as V

import qualified Data.Map as M

import Prelude hiding (dropWhile)

data Tile = Tile Int (Vector (Vector Char)) deriving Show

parser :: Parser String [Tile]
parser = many (dropWhile isSpace *> tile) <* dropWhile isSpace
    where
    tile = Tile <$> (string "Tile " *> nat <* char ':' <* endOfLine)
                <*> V.replicateM 10 (V.fromListN 10 <$> line)

edges :: Tile -> [Vector Char]
edges tile = original ++ reversed
    where
    original = [top tile, bottom tile, left tile, right tile]
    reversed = V.reverse <$> original

top, bottom, left, right :: Tile -> Vector Char
top    (Tile _ xss) = xss ! 0
bottom (Tile _ xss) = xss ! 9
left   (Tile _ xss) = (\i -> xss ! i ! 0) <$> V.iterateN 10 (+1) 0
right  (Tile _ xss) = (\i -> xss ! i ! 9) <$> V.iterateN 10 (+1) 0

main :: IO ()
main = do

    Right input <- parse parser <$> readFile "./day20/sample_input_1"

    print $ part1 input
    part2 input

    where
    part1 = product . cornerIds

part2 :: [Tile] -> IO ()
part2 tiles = do
    mapM_ print . connections $ tiles
    print . head $ cornerIds tiles

cornerIds :: [Tile] -> [Int]
cornerIds = map head
          . filter (\x -> length x == 4)
          . group
          . sort
          . concatMap snd
          . filter (\(_,ts) -> length ts == 1)
          . M.toList
          . foldr (\(n,e) -> M.insertWith (++) e [n]) M.empty
          . concatMap (\t@(Tile n _) -> zip (repeat n) (edges t))

connections :: [Tile] -> [(Vector Char, [Int])]
connections = M.toList
            . foldr (\(n,e) -> M.insertWith (++) e [n]) M.empty
            . concatMap (\t@(Tile n _) -> zip (repeat n) (edges t)) --more info

-- remember to remove borders