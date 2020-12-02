import Common.Parser
import Common.Parser.String
import Common.Parser.Combinator

data Rule = Rule { mn :: !Int
                 , mx :: !Int
                 , cr :: !Char
                 } deriving Show

newtype Pword =
    Pword String deriving Show

_sampleInput :: String
_sampleInput = "1-3 a: abcde\n\
               \1-3 b: cdefg\n\
               \2-9 c: ccccccccc"

parseInput :: Parser String [(Rule, Pword)]
parseInput = many $ do

    rule <- Rule <$> int     <* char '-'
                 <*> int     <* char ' '
                 <*> oneChar <* string ": "

    pword <- Pword <$> line

    pure (rule, pword)

isValid1 :: (Rule, Pword) -> Bool
isValid1 (rule, Pword pword) = occurrences >= mn rule
                            && occurrences <= mx rule
    where occurrences = length . filter (== cr rule) $ pword

isValid2 :: (Rule, Pword) -> Bool
isValid2 (rule, Pword pword) = (cr rule == pword !! (mn rule - 1))
                            /= (cr rule == pword !! (mx rule - 1))

main :: IO ()
main = do

    Right input <- parse parseInput <$> readFile "day02/input"

    print (part1 input)

    print (part2 input)

    where
    part1 = length . filter id . map isValid1

    part2 = length . filter id . map isValid2