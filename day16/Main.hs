{-# LANGUAGE TupleSections #-}

import Common.Parser
import Common.Parser.Combinator
import Common.Parser.String

import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set as S

import Data.List (groupBy, isPrefixOf, transpose)

_sampleInput1 :: String
_sampleInput1 = "class: 1-3 or 5-7\n\
                \row: 6-11 or 33-44\n\
                \seat: 13-40 or 45-50\n\
                \\n\
                \your ticket:\n\
                \7,1,14\n\
                \\n\
                \nearby tickets:\n\
                \7,3,47\n\
                \40,4,50\n\
                \55,2,20\n\
                \38,6,12"

_sampleInput2 :: String
_sampleInput2 = "class: 0-1 or 4-19\n\
                \row: 0-5 or 8-19\n\
                \seat: 0-13 or 16-19\n\
                \\n\
                \your ticket:\n\
                \11,12,13\n\
                \\n\
                \nearby tickets:\n\
                \3,9,18\n\
                \15,1,5\n\
                \5,14,9"

newtype Ticket =
    Ticket [Int]

data Range =
    Range !Int !Int

data Rule =
    Rule !String ![Range]

data Problem =
    Problem ![Rule] !Ticket ![Ticket]

newtype Col =
    Col Int deriving (Eq, Ord)

data Contradiction =
    Contradiction !Col !String

parser :: Parser String Problem
parser = do
    rs  <- many rule
    _   <- line
    _   <- string "your ticket:" <* endOfLine
    yt  <- Ticket <$> natList    <* endOfLine
    _   <- line
    _   <- string "nearby tickets:" <* endOfLine
    ots <- map Ticket <$> many (natList <* end)
    pure $ Problem rs yt ots

    where
    rule = do
        name <- takeWhile1 (/=':')
        _    <- string ": "
        rng  <- range
        rngs <- many (string " or " *> range)
        _    <- endOfLine
        pure $ Rule name (rng:rngs)

        where
        range = Range <$> nat <* char '-'
                      <*> nat

    natList = (:) <$> nat
                  <*> many (char ',' *> nat)

main :: IO ()
main = do
    Right input <- parse parser <$> readFile "day16/input"
    print $ part1 input
    print $ part2 input

part1 :: Problem -> Int
part1 (Problem rules _ nearTickets) =
    sum $ concatMap (findInvalidFields rules) nearTickets

part2 :: Problem -> Int
part2 (Problem rules yourTicket allTickets) = do

    let validTickets = filter (not . isInvalid rules) (yourTicket:allTickets)

        ticketMatrix = transpose $ (\(Ticket xs) -> zip (Col <$> [0..]) xs) <$> validTickets

        possibilities = concat ((\n -> map (n,)) <$> rules <*> ticketMatrix)

        known = reduce possibilities

    product $ relevantFields known yourTicket

isInvalid :: [Rule] -> Ticket -> Bool
isInvalid rules = not . null . findInvalidFields rules

findInvalidFields :: [Rule] -> Ticket -> [Int]
findInvalidFields rules (Ticket fs) =
    let rngss = (\(Rule _ rngs) -> rngs) <$> rules
    in [f | f <- fs, not (any (inDisjointRanges f) rngss)]

inDisjointRanges :: Int -> [Range] -> Bool
inDisjointRanges f = any inRange where inRange (Range a b) = f >= a && f <= b

relevantFields :: Map String Col -> Ticket -> [Int]
relevantFields known (Ticket fs) =

    let cols = map (\(_, Col c) -> c)
             . filter (\(n, _) -> "departure" `isPrefixOf` n)
             $ M.toList known

    in map (fs !!) cols

reduce :: [(Rule, (Col, Int))] -> Map String Col
reduce = go M.empty

    where
    go known            [] = known
    go known possibilities =
            -- Find contractions
        let contradictions = mapMaybe findContradiction possibilities

            -- Apply contradictions
            possibilities' = filter (not . isContradicted contradictions) possibilities

            -- Find truisms
            newKnown = M.fromList $ findTruisms possibilities'

            -- Cull via truisms
            possibilities'' = cull newKnown possibilities'

        in go (known <> newKnown) possibilities''

    findContradiction (Rule name rngs, (col, f))
        | inDisjointRanges f rngs = Nothing
        | otherwise               = Just $ Contradiction col name

    isContradicted contradictions (Rule n _, (c, _)) =
        any (\(Contradiction col name) -> c == col && name == n) contradictions

    findTruisms = mapMaybe findTruism
                . groupBy (\a b -> fst a == fst b)
                . map (\(Rule name _, (col, _)) -> (name, col))
        where
        findTruism         []                         = Nothing
        findTruism ((n,c):xs) | all ((== c) . snd) xs = Just (n, c)
                              | otherwise             = Nothing

    cull known possibilities =

        let names = M.keysSet known
            cols  = S.fromList . map snd . M.toList $ known

        in filter (not . cull' names cols) possibilities

            where
            cull' names cols (Rule n _, (c, _)) = n `S.member` names
                                               || c `S.member` cols



