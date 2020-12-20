import Common.Parser
import Common.Parser.Combinator
import Common.Parser.String

import           Control.Applicative     ((<|>))
import           Data.IntMap.Strict      (IntMap)
import qualified Data.IntMap.Strict as M

import Prelude hiding (dropWhile)

data Rule = Match !Char
          | Rules ![[Int]]

_sampleInput :: String
_sampleInput = "0: 4 1 5\n\
               \1: 2 3 | 3 2\n\
               \2: 4 4 | 5 5\n\
               \3: 4 5 | 5 4\n\
               \4: \"a\"\n\
               \5: \"b\"\n\
               \\n\
               \ababbb\n\
               \bababa\n\
               \abbbab\n\
               \aaabbb\n\
               \aaaabbb"

parser :: Parser [Char] (IntMap Rule, [String])
parser = do
    _  <- dropWhile (== ' ')
    rs <- many1 (rule <* endOfLine)
    ls <- line *> many1 line
    pure (M.fromList rs, ls)

    where
    rule = (,) <$> nat <* string ": "
               <*> (rules <|> match)

    rules = Rules <$> many1 segment

    segment = opt (=='|') *> dropWhile (== ' ') *> many1 (nat <* dropWhile (== ' '))

    match = Match 'a' <$ string "\"a\""
        <|> Match 'b' <$ string "\"b\""

accepts :: IntMap Rule -> Int -> String -> Bool
accepts rules m ys = ("" `elem`) $ go m ys

    where
    go :: Int -> String -> [String]
    go n xs =

        case M.lookup n rules of

            Nothing -> error "invalid rule"

            Just (Match c) | null xs      -> []
                           | head xs == c -> [tail xs]
                           | otherwise    -> []

            Just (Rules rs) -> concatMap (`seqRules` xs) rs

    seqRules :: [Int] -> String -> [String]
    seqRules     [] xs = [xs]
    seqRules (n:ns) xs = concatMap (seqRules ns) $ go n xs

main :: IO ()
main = do

    Right input <- parse parser <$> readFile "./day19/input"

    print $ part1 input

    print $ part2 input

    where
    part1 (rules, strings) = length
                           . filter id
                           . map (accepts rules 0)
                           $ strings

    part2 (rules, strings) =
        let rules' = M.insert  8 (Rules [    [42],      [42, 8]])
                   . M.insert 11 (Rules [[42, 31], [42, 11, 31]])
                   $ rules

        in part1 (rules', strings)
