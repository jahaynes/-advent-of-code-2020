import           Data.Set      (Set)
import qualified Data.Set as S

_sampleInput :: String
_sampleInput = ".#.\n\
               \..#\n\
               \###"

parse2dInputZYX :: String -> Set (Int, Int, Int)
parse2dInputZYX str = S.fromList $ do
    let ls     = lines str
        width  = length $ head ls
        height = length ls
        depth  = 1
    z <- [0..depth  - 1]
    y <- [0..height - 1]
    x <- [0..width  - 1]
    case ls !! y !! x of
        '#' -> [(z,y,x)]
        _   -> []

parse2dInputWZYX :: String -> Set (Int, Int, Int, Int)
parse2dInputWZYX str = S.fromList $ do
    let ls       = lines str
        width    = length $ head ls
        height   = length ls
        depth    = 1
        duration = 1
    w <- [0..duration - 1]
    z <- [0..depth    - 1]
    y <- [0..height   - 1]
    x <- [0..width    - 1]
    case ls !! y !! x of
        '#' -> [(w,z,y,x)]
        _   -> []

boundsZYX :: Set (Int, Int, Int)
          -> ((Int, Int), (Int, Int), (Int, Int))
boundsZYX =
    go (0, 0) (0, 0) (0, 0) . S.toList
    where
    go (zl, zh) (yl, yh) (xl, xh)           [] = ( (zl-1, zh+1)
                                                 , (yl-1, yh+1)
                                                 , (xl-1, xh+1) )
    go (zl, zh) (yl, yh) (xl, xh) ((z,y,x):cs) =
        let zl' = min z zl
            zh' = max z zh
            yl' = min y yl
            yh' = max y yh
            xl' = min x xl
            xh' = max x xh
        in go (zl', zh') (yl', yh') (xl', xh') cs

boundsWZYX :: Set (Int, Int, Int, Int)
          -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int))
boundsWZYX =
    go (0, 0) (0, 0) (0, 0) (0, 0) . S.toList
    where
    go (wl, wh) (zl, zh) (yl, yh) (xl, xh)  [] = ( (wl-1, wh+1)
                                                 , (zl-1, zh+1)
                                                 , (yl-1, yh+1)
                                                 , (xl-1, xh+1) )
    go (wl, wh) (zl, zh) (yl, yh) (xl, xh) ((w,z,y,x):cs) =
        let wl' = min w wl
            wh' = max w wh
            zl' = min z zl
            zh' = max z zh
            yl' = min y yl
            yh' = max y yh
            xl' = min x xl
            xh' = max x xh
        in go (wl', wh') (zl', zh') (yl', yh') (xl', xh') cs

neighboursZYX :: (Int, Int, Int)
              -> [(Int, Int, Int)]
neighboursZYX o@(z, y, x) =
    filter (/= o) $ do z' <- [z-1..z+1]
                       y' <- [y-1..y+1]
                       x' <- [x-1..x+1]
                       pure (z', y', x')

neighboursWZYX :: (Int, Int, Int, Int)
              -> [(Int, Int, Int, Int)]
neighboursWZYX o@(w, z, y, x) =
    filter (/= o) $ do w' <- [w-1..w+1]
                       z' <- [z-1..z+1]
                       y' <- [y-1..y+1]
                       x' <- [x-1..x+1]
                       pure (w', z', y', x')

stepZYX :: Set (Int, Int, Int)
        -> Set (Int, Int, Int)
stepZYX state = S.fromList $ do

    z <- [zl..zh]
    y <- [yl..yh]
    x <- [xl..xh]

    let activeNeighbours = length
                         . filter (`S.member` state)
                         . neighboursZYX
                         $ (z, y, x)

    if activeNeighbours == 3 || S.member (z,y,x) state && activeNeighbours == 2
        then [(z,y,x)]
        else []

    where
    ((zl, zh), (yl, yh), (xl, xh)) = boundsZYX state

stepWZYX :: Set (Int, Int, Int, Int)
         -> Set (Int, Int, Int, Int)
stepWZYX state = S.fromList $ do

    (w, z, y, x) <- candidates

    let activeNeighbours = length
                         . filter (`S.member` state)
                         . neighboursWZYX
                         $ (w, z, y, x)

    if activeNeighbours == 3 || S.member (w, z,y,x) state && activeNeighbours == 2
        then [(w, z,y,x)]
        else []

    where
    ((wl, wh), (zl, zh), (yl, yh), (xl, xh)) = boundsWZYX state

    candidates = do w <- [wl..wh]
                    z <- [zl..zh]
                    y <- [yl..yh]
                    x <- [xl..xh]
                    pure (w, z, y, x)

    candidates' = S.toList . S.fromList . concatMap (\p -> p:neighboursWZYX p) . S.toList

main :: IO ()
main = do

    input <- readFile "./day17/input"

    --print $ part1 input
    print $ part2 input

    where
    --part1 = S.size . last . take 7 . iterate stepZYX . parse2dInputZYX
    part2 = S.size . last . take 7 . iterate stepWZYX . parse2dInputWZYX
