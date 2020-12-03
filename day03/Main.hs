{-# LANGUAGE BangPatterns #-}

_sampleInput :: String
_sampleInput = "..##.......\n\
               \#...#...#..\n\
               \.#....#..#.\n\
               \..#.#...#.#\n\
               \.#...##..#.\n\
               \..#.##.....\n\
               \.#.#.#....#\n\
               \.#........#\n\
               \#.##...#...\n\
               \#...##....#\n\
               \.#..#...#.#"

go :: Int -> (Int, Int) -> [String] -> Int
go !acc     _   [] = acc
go  acc slope grid = go (acc+tree) slope (step slope grid)
    where
    tree = if head (head grid) == '#' then 1 else 0
    step (d, r) = map (drop r) . drop d

main :: IO ()
main = do

    rawInput <- readFile "day03/input"

    let input = map (concat . repeat) . lines $ rawInput

    print $ part1 input

    print $ part2 input

    where
    part1 grid = go 0 (1, 3) grid

    part2 grid = product $ map (\slope -> go 0 slope grid) [(1,1), (1,3), (1,5), (1,7), (2,1)]
