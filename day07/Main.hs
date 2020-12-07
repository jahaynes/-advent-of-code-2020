import Common.Parser
import Common.Parser.Combinator
import Common.Parser.String

import           Control.Applicative ((<|>))
import           Data.Char
import           Data.Map            (Map)
import qualified Data.Map.Strict as M
import           Data.Set            (Set)
import qualified Data.Set as S

import Prelude hiding (dropWhile)

{- First we lex the input string into Tokens -}

data Token = TokComma
           | TokDot
           | TokNum Int
           | TokBag
           | TokContain
           | TokDesc String
               deriving (Eq, Show)

lexer :: Parser String [Token]
lexer = dropWhile isSpace *> many1 (token <* dropWhile isSpace)
    where
    token = TokComma   <$  char ','
        <|> TokDot     <$  char '.'
        <|> TokNum     <$> (string "no other" *> pure 0 <|> int )
        <|> TokBag     <$  (string "bags" <|> string "bag")
        <|> TokContain <$  (string "contains" <|> string "contain")
        <|> TokDesc    <$> takeWhile1 isAlpha

{- Then we parse the Tokens into Rules -}

data Rule = Rule { getLhs  :: !Bag
                 , getRhss :: !(Map Bag Int)
                 }

newtype Bag =
    Bag (Set String)
        deriving (Eq, Ord)

parser :: Parser [Token] [Rule]
parser = many $ do
    lhs  <- takeUntil (== TokContain) <* such (== TokContain)
    rhs  <- firstClause
    rhss <- many remainingClause <* such (== TokDot)
    pure $ Rule { getLhs  = asBag lhs
                , getRhss = M.fromList . map (\(TokNum n:descs) -> (asBag descs, n)) $ rhs:rhss
                }

    where
    firstClause :: Parser [Token] [Token]
    firstClause = takeUntil (\t -> t == TokComma || t == TokDot)

    remainingClause :: Parser [Token] [Token]
    remainingClause = such (== TokComma) *> firstClause

    asBag :: [Token] -> Bag
    asBag = Bag
          . S.fromList
          . map (\(TokDesc s) -> s)
          . filter (/= TokBag)

_sampleInput1 :: String
_sampleInput1 = "light red bags contain 1 bright white bag, 2 muted yellow bags.\n\
                \dark orange bags contain 3 bright white bags, 4 muted yellow bags.\n\
                \bright white bags contain 1 shiny gold bag.\n\
                \muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\n\
                \shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\n\
                \dark olive bags contain 3 faded blue bags, 4 dotted black bags.\n\
                \vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\n\
                \faded blue bags contain no other bags.\n\
                \dotted black bags contain no other bags."

_sampleInput2 :: String
_sampleInput2 = "shiny gold bags contain 2 dark red bags.\n\
                \dark red bags contain 2 dark orange bags.\n\
                \dark orange bags contain 2 dark yellow bags.\n\
                \dark yellow bags contain 2 dark green bags.\n\
                \dark green bags contain 2 dark blue bags.\n\
                \dark blue bags contain 2 dark violet bags.\n\
                \dark violet bags contain no other bags."

main :: IO ()
main = do

    input <- readFile "day07/input"

    let Right rules = parse lexer input >>= parse parser

    let ruleMap = M.fromList . map (\(Rule l r) -> (l, r)) $ rules

    part1 ruleMap

    part2 ruleMap

    where
    part1 :: Map Bag (Map Bag a) -> IO ()
    part1 ruleMap = print
                  . S.size
                  . S.unions
                  . map (go S.empty)
                  . S.toList
                  . M.keysSet
                  $ ruleMap

        where
        go acc bag =

            case M.lookup bag ruleMap of

                Just targetMap ->
                    let acc'  = S.insert bag acc
                        bags' = S.toList . M.keysSet $ targetMap
                    in
                    if any (== shinyGoldBag) bags'
                        then acc'
                        else S.unions $ map (go acc') bags'

                Nothing -> S.empty

    part2 :: Map Bag (Map Bag Int) -> IO ()
    part2 ruleMap = print $ go shinyGoldBag
                          - 1 -- Don't include original bag
        where
        go bag =

            case M.lookup bag ruleMap of

                -- Count this bag
                Nothing -> 1

                -- Count this bag and every bag it holds
                Just rules -> 1
                            + (sum . map (\(bag', c) -> c * go bag') . M.toList $ rules)

    shinyGoldBag :: Bag
    shinyGoldBag = Bag $ S.fromList ["shiny", "gold"]