{-# LANGUAGE ScopedTypeVariables #-}

import Common.Parser
import Common.Parser.Combinator
import Common.Parser.String

import Control.Monad (join)

import           Data.Char        (isSpace)
import           Data.List        (transpose, group, sort, sortBy)

import Data.Ord   (comparing)
import Data.Maybe (fromJust, catMaybes)

import           Data.Map         (Map)
import qualified Data.Map as M
import           Data.Set         (Set, (\\))
import qualified Data.Set as S
import           Data.Vector      (Vector, (!))
import qualified Data.Vector as V

import Prelude hiding (dropWhile)

data Tile = Tile !Int !(Vector (Vector Char)) deriving Show

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

shrink :: Tile -> Tile
shrink (Tile n xss) = Tile n $ V.map (V.init . V.tail) . V.init . V.tail $ xss

newtype Monster =
    Monster (Vector (Vector Char))

monster :: Monster
monster = Monster
        $ V.fromList
      <$> V.fromList
        ["                  # "
        ,"#    ##    ##    ###"
        ," #  #  #  #  #  #   "]

mWidth, mHeight :: Int
mWidth  = (\(Monster m) -> V.length (m ! 0)) monster
mHeight = (\(Monster m) -> V.length m      ) monster

countAllAlignmentMonsters :: Monster -> Tile -> Int
countAllAlignmentMonsters (Monster m) tile@(Tile _ xss') =

    let height  = V.length xss'
        width   = V.length (xss' ! 0)

        maxStartY = height - mHeight
        maxStartX = width  - mWidth

    in maximum $ map (countMonsters maxStartX maxStartY (join m)) (alignments tile)

    where
    countMonsters maxStartY maxStartX monster' (Tile _ xss) = do

        let subgrids = catMaybes $ getSubgrid <$> [0..maxStartX]
                                              <*> [0..maxStartY]

        length . filter id
               . map (V.all id . V.zipWith (\m' b -> if m' == '#' then b == '#' else True) monster' . join)
               $ subgrids

        where
        getSubgrid :: Int -> Int -> Maybe (Vector (Vector Char))
        getSubgrid sy sx = 
            let g = V.take mWidth . V.drop sx <$> (V.take mHeight $ V.drop sy xss)
            in if V.length g == mHeight && V.length (g!0) == mWidth
                   then Just g
                   else Nothing

main :: IO ()
main = do

    Right input <- parse parser <$> readFile "./day20/input"

    print $ part1 input
    part2 input

    where
    part1 :: [Tile] -> Int
    part1 = product . cornerIds

    part2 :: [Tile] -> IO ()
    part2 tiles = do

        let tileMap = M.fromList $ map (\t@(Tile n _) -> (n, t)) tiles
            conns   = connections tiles

        let startId = head $ cornerIds tiles

            places  = placements conns M.empty (S.singleton startId) (Righ, 0::Int, 0::Int) startId 

        print $ M.size places

        let placess = map (placements conns M.empty (S.singleton startId) (Righ, 0::Int, 0::Int)) (cornerIds tiles)

        mapM_ (print . M.size) placess



{-
            places' = fromJust . (`M.lookup` tileMap) <$> places
            sideLength = floor . sqrt . fromIntegral . length $ tiles
            aligned = fixAlignment sideLength conns places <$> M.toList places'

        let shrunk = fmap shrink <$> aligned

        let big = merge $ M.fromList shrunk   

        let numMonsters = countAllAlignmentMonsters monster big

        let hashesPerMonster = let Monster m = monster in V.length . V.filter (=='#') $ join m

        let monsterHashes = numMonsters * hashesPerMonster

        let gridHashes = let (Tile _ xss) = big in V.length . V.filter (=='#') $ join xss

        print $ gridHashes - monsterHashes

-}

data Dir = Up | Righ | Down | Lef

maxCoords :: Map (Int, Int) Tile -> (Int, Int)
maxCoords tiles = do
    let coords = M.keys tiles
        ys     = fst <$> coords
        xs     = snd <$> coords
    (maximum ys, maximum xs)

merge :: Map (Int, Int) Tile -> Tile
merge tiles =
    let (maxy, maxx) = maxCoords tiles
        height     = maxy + 1
        width      = maxx + 1
        tileHeight = 8
        tileWidth  = 8
        bigHeight = tileHeight * height
        bigWidth  = tileWidth * width
        big = V.generate bigHeight $ \h ->
                  V.generate bigWidth $ \w ->
                      let (tiley, y) = h `divMod` tileHeight
                          (tilex, x) = w `divMod` tileWidth
                          Just (Tile _ xss) = M.lookup (tiley, tilex) tiles
                      in xss ! y ! x
    in Tile (-1) big

fixAlignment sideLength conns places ((y,x),xss) =

    let Just tile' = alignTileSuchThat xss $ \tile -> and [ consistentWithUp    tile
                                                          , consistentWithLeft  tile
                                                          , consistentWithDown  tile
                                                          , consistentWithRight tile
                                                          ]

    in ((y,x), tile')

    where
    consistentWithUp tile = y == 0
                         || let Just up   = M.lookup (y-1, x) places
                                Just edge = M.lookup (top tile) conns
                            in up `elem` edge

    consistentWithLeft tile = x == 0
                           || let Just lef  = M.lookup (y, x-1) places
                                  Just edge = M.lookup (left tile) conns
                              in lef `elem` edge

    consistentWithDown tile = y == sideLength - 1
                           || let Just down = M.lookup (y+1, x) places
                                  Just edge = M.lookup (bottom tile) conns
                              in down `elem` edge

    consistentWithRight tile = x == sideLength - 1
                            || let Just righ = M.lookup (y, x+1) places
                                   Just edge = M.lookup (right tile) conns
                               in righ `elem` edge

alignTileSuchThat :: Tile -> (Tile -> Bool) -> Maybe Tile
alignTileSuchThat tile f = headMay . filter f . alignments $ tile

rotate :: Tile -> Tile
rotate (Tile n xss) =
    Tile n . fmap V.fromList
           . V.fromList
           . map reverse
           . transpose
           . fmap V.toList
           $ V.toList xss

alignments :: Tile -> [Tile]
alignments tile = do
    r <- rotations
    f <- flips
    pure $ f (r tile)
    where
    rotations :: [Tile -> Tile]
    rotations = map (\n -> last . take n . iterate rotate) [1..4]

    flips :: [Tile -> Tile]
    flips = [ id
            , (\(Tile n xs) -> Tile n (V.reverse     xs))
            , (\(Tile n xs) -> Tile n (V.reverse <$> xs))
            ]

placements conns layout visited (dir, x, y) chosen = do

    let layout' = M.insert (x,y) chosen layout

    let outs = S.toList (outnodes conns chosen \\ visited)

    let dir' = if length outs == 1
                    then next dir
                    else dir

    let x' = case dir' of
                    Righ -> x + 1
                    Lef  -> x - 1
                    _    -> x

    let y' = case dir' of
                    Down -> y + 1
                    Up   -> y - 1
                    _    -> y


    case headMay $ sortBy (comparing (\o -> length (outnodes conns o))) outs of     -- outnodes, not outnodes NOT YET VISITED?
        Nothing  -> layout'
        Just out -> placements conns layout' (S.insert out visited) (dir', x', y') out 

    where
    next :: Dir -> Dir
    next   Up = Righ
    next Righ = Down
    next Down = Lef
    next  Lef = Up

outnodes :: Ord a => Map k [a] -> a -> Set a
outnodes conns chosen = S.fromList
                      . filter (/=chosen)
                      . concatMap snd
                      . M.toList
                      . M.filter (chosen `elem`)
                      $ conns

headMay :: [a] -> Maybe a
headMay    [] = Nothing
headMay (x:_) = Just x

cornerIds :: [Tile] -> [Int]
cornerIds = map head
          . filter (\x -> length x == 4)
          . group
          . sort
          . concatMap snd
          . filter (\(_,ts) -> length ts == 1)
          . M.toList    
          . connections

connections :: [Tile] -> Map (Vector Char) [Int]
connections = foldr (\(n,e) -> M.insertWith (++) e [n]) M.empty
            . concatMap (\t@(Tile n _) -> zip (repeat n) (edges t))
