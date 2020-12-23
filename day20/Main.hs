{-# LANGUAGE ScopedTypeVariables, TupleSections #-}

import Common.Parser
import Common.Parser.Combinator
import Common.Parser.String

import Control.Monad (forM_, join)

import           Data.Char        (isSpace)
import           Data.List        (delete, transpose, group, sort, sortBy, (\\))

import Data.Ord   (comparing)
import Data.Maybe (isJust, fromJust, catMaybes)

import           Data.Map         (Map)
import qualified Data.Map as M
import           Data.Set         (Set)
import qualified Data.Set as S
import           Data.Vector      (Vector, (!))
import qualified Data.Vector as V

import Prelude hiding (dropWhile)

data Tile = Tile !Int !(Vector (Vector Char)) deriving (Eq, Ord, Show)

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

properEdges :: Tile -> [Vector Char]
properEdges tile = [top tile, bottom tile, left tile, right tile]

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

        let alignedTileMap = fixAllAlignments tileMap tiles conns startId 

        let alignedConns = properConnections . map snd . M.toList $ alignedTileMap

        let unplaced = map (\(Tile n _) -> n) tiles

        placeAlignedTiles alignedConns alignedTileMap startId (delete startId unplaced)



        

{-
        let places' = fromJust . (`M.lookup` tileMap) <$> places
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

placeAlignedTiles :: Map (Vector Char) [Int]
                  -> Map Int Tile
                  -> Int
                  -> [Int]
                  -> IO ()
placeAlignedTiles conns alignedTileMap st = go M.empty [((0,0), st)]

    where
    go :: Map (Int,Int) Int -> [((Int, Int), Int)] -> [Int] -> IO ()
    go placed ts unplaced = do
        
        let placed' = foldr (\(yx,t) -> M.insert yx t) placed ts

        case concatMap findNeighboursFor ts of

            []         -> pure ()
            neighbours -> do

                --- mapM_ print neighbours

                let unplacedNeigboughs = S.toList . S.fromList $ filter (\(yx,_) -> not . isJust . M.lookup yx $ placed) neighbours

                print "Unplaced"
                mapM_ print unplacedNeigboughs
                putStrLn "----"

                let neighboursc = map (\(yx, Tile n _) -> (yx,n)) unplacedNeigboughs
                let neighboursi = map (\(_, Tile n _) -> n) unplacedNeigboughs


                go placed' neighboursc (unplaced \\ neighboursi)
                
        where
        findNeighboursFor ((y,x), t) = do

            let Just tile = M.lookup t alignedTileMap

            catMaybes [ ((y-1,  x),) <$> findNeighbourBy top tile
                      , ((  y,x+1),) <$> findNeighbourBy right tile
                      , ((y+1,  x),) <$> findNeighbourBy bottom tile
                      , ((  y,x-1),) <$> findNeighbourBy left tile
                      ]

        findNeighbourBy :: (Tile -> Vector Char) -> Tile -> Maybe Tile
        findNeighbourBy dir tile@(Tile n _) = do
            x <- M.lookup (dir tile) conns
            u <- case filter (\x -> x `elem` unplaced) x of
                    []  -> Nothing
                    [x] -> Just x
            M.lookup u alignedTileMap

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

{-
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
-}
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

{-
fixAllAlignments :: Map Int Tile
            -> [Tile]
            -> Map (Vector Char) [Int]
            -> Int
            -> (Map (Int, Int) Int) -}
fixAllAlignments tileMap tiles conns chosen = do

    --arbitrary choice
    let Just cornerTile = M.lookup (cornerIds tiles !! 1) tileMap  

    --align it nicely as TL (also arbitrary)
    let Just act@(Tile actn _) =
            alignTileSuchThat cornerTile $ \ct ->
                and [ (length . fromJust . M.lookup (right  ct) $ conns) == 1
                    , (length . fromJust . M.lookup (bottom ct) $ conns) == 1
                    ]

    let tileNums = (\(Tile n _) -> n) <$> tiles

    go (S.singleton act) (delete actn tileNums)
    
    where
    -- go :: Int -> Map Tile (Int, Int) -> [Int] -> (Map (Int, Int) Int)
    go placed       [] = M.fromList . map (\t@(Tile n _) -> (n,t)) $ S.toList placed
    go placed unplaced = do

        let placed' = (\(Tile n _) -> n) <$> S.toList placed

        let (cn, jcn) =
                head $ do p <- placed'
                          u <- unplaced
                          if attached conns p u
                              then [(p, u)]
                              else []

        let Just chosen = M.lookup cn tileMap

        let Just joinCandidate = M.lookup jcn tileMap

        let Just ajc = alignJoiner chosen joinCandidate

        go (S.insert ajc placed) (delete jcn unplaced)

        where
        alignJoiner chosen joiner =
            headMay . catMaybes $ [ alignTileSuchThat joiner $ \t -> right chosen == left t
                                  , alignTileSuchThat joiner $ \t -> left chosen == right t
                                  , alignTileSuchThat joiner $ \t -> top chosen == bottom t
                                  , alignTileSuchThat joiner $ \t -> bottom chosen == top t
                                  ]

    {-
        The t<->nc alignment should be strong enough to force nc into the correct rotation!
            Is this true?
        
    
    -}

        
    recoord :: Map (Int, Int) Int -> Map (Int, Int) Int
    recoord m = do
        let ys = fst <$> M.keys m
        let xs = snd <$> M.keys m
        let minY = abs $ min 0 (minimum ys)
        let minX = abs $ min 0 (minimum xs)
        M.fromList . map (\((y,x), t) -> ((y+minY,x+minX), t)) . M.toList $ m


        -- is t attached to an unvisited 
{-
    classify = classify' mempty mempty mempty
        where
        classify' corners edges other     [] = (corners, edges, other)
        classify' corners edges other (t:ts) = 
            case S.size (outnodes conns t) of
                4 -> classify' corners edges (S.insert t other) ts
                3 -> classify' corners (S.insert t edges) other ts
                2 -> classify' (S.insert t corners) edges other ts
-}
attached :: Map k [Int] -> Int -> Int -> Bool
attached conns a b = a /= b
                  && (not . null . M.filter (\ts -> a `elem` ts && b `elem` ts) $ conns)

{-
    let visited' = S.insert chosen visited

    print chosen

    let nextChosenCandidates = S.toList (outnodes conns chosen \\ visited')

    
    let options' = map (\nc -> (nc, length (outnodes conns nc \\ visited')) ) nextChosenCandidates

    let options = sortBy (comparing (\nc -> length (outnodes conns nc \\ visited'))) nextChosenCandidates

    print options'
    print options

    putStrLn ""

    case headMay options of

        Nothing -> pure n

        Just nc -> placements2 (n+1) conns  visited'  nc


    forM_ nextChosenCandidates $ \nc -> do

        print (nc, length (outnodes conns nc \\ visited'))
-}

    {-
2311
[1427,3079]
is wrong
-}


{-
placements :: Map (Vector Char) [Int]
           -> Map (Int, Int) Int
           -> Set Int
           -> (Dir, Int, Int)
           -> Int
           -> IO (Map (Int, Int) Int)
placements conns layout visited (dir, x, y) chosen = do

    let layout' = M.insert (x,y) chosen layout

    let outs = S.toList (outnodes conns chosen \\ visited)

    print (chosen, outs)

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

    case headMay $ sortBy (comparing (length . outnodes conns)) outs of     -- outnodes, not outnodes NOT YET VISITED?
        Nothing  -> pure layout'
        Just out -> placements conns layout' (S.insert out visited) (dir', x', y') out 

    where
    next :: Dir -> Dir
    next   Up = Righ
    next Righ = Down
    next Down = Lef
    next  Lef = Up
-}

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

cornerIds :: [Tile] -> [Int]    -- Don't need this, see classify?
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


properConnections :: [Tile] -> Map (Vector Char) [Int]
properConnections = foldr (\(n,e) -> M.insertWith (++) e [n]) M.empty
                  . concatMap (\t@(Tile n _) -> zip (repeat n) (properEdges t))

