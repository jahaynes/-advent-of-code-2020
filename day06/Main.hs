import Data.List (foldl1', groupBy)
import Data.Set  (size, fromList, intersection)

_sampleInput :: String
_sampleInput = "abc\n\
               \\n\
               \a\n\
               \b\n\
               \c\n\
               \\n\
               \ab\n\
               \ac\n\
               \\n\
               \a\n\
               \a\n\
               \a\n\
               \a\n\
               \\n\
               \b"

main :: IO ()
main = do

    input <- lines <$> readFile "day06/input"

    part1 input

    part2 input

    where
    part1 = print
          . sum
          . map (size . fromList)
          . filter (not . null)
          . map concat
          . groupBy (\a b -> null a == null b)

    part2 = print
          . sum
          . map (size . foldl1' intersection . map fromList)
          . filter (not . null . head)
          . groupBy (\a b -> null a == null b)