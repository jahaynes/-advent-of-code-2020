{-# LANGUAGE BangPatterns,
             OverloadedLists,
             MultiWayIf #-}

import Common.Parser
import Common.Parser.Combinator
import Common.Parser.String

import           Control.Applicative ((<|>))
import           Data.Char
import           Data.Either         (rights)
import qualified Data.IntSet as IS
import           Data.Vector         ((!), Vector)
import qualified Data.Vector as V
import           Text.Printf         (printf)

import Prelude hiding (dropWhile)

data Instr =
    Nop | Acc | Jmp

newtype Arg =
    Arg Int

data Computer =
    Computer { ip  :: !Int
             , acc :: !Int
             }

newtype Program =
    Program (Vector (Instr, Arg))

parser :: Parser String Program
parser = Program . V.fromList <$> lins
    where
    lins = dropWhile isSpace *> many1 (lin <* dropWhile isSpace)
    lin = do
        i <- instr <* dropWhile isSpace
        a <- arg
        pure (i, a)

    instr = Nop <$ string "nop"
        <|> Acc <$ string "acc"
        <|> Jmp <$ string "jmp"

    arg = Arg          <$> (char '+' *> nat)
      <|> Arg . negate <$> (char '-' *> nat)

run :: Program -> Computer -> Either String Int
run (Program prog) = go mempty
    where
    go !ran comp =
        let iptr = ip comp
        in
        if | iptr == V.length prog -> Right $ acc comp
           | iptr `IS.member` ran  -> Left $ printf "Looped! acc was: %d" (acc comp)
           | otherwise             ->
                let instr = prog ! iptr
                    ran'  = IS.insert iptr ran
                    comp' = step comp instr
                in
                go ran' comp'

    step :: Computer -> (Instr, Arg) -> Computer
    step comp (Nop,     _) = comp { ip  = ip  comp + 1 }
    step comp (Acc, Arg x) = comp { ip  = ip  comp + 1 
                                  , acc = acc comp + x }
    step comp (Jmp, Arg x) = comp { ip  = ip  comp + x }

_sampleInput :: String
_sampleInput = "nop +0\n\
               \acc +1\n\
               \jmp +4\n\
               \acc +3\n\
               \jmp -3\n\
               \acc -99\n\
               \acc +1\n\
               \jmp -4\n\
               \acc +6"

mutations :: Program -> [Program]
mutations (Program prog) = do
    i <- [0..V.length prog-1]
    case prog ! i of
        (Jmp, x) -> [Program $ V.update prog [(i, (Nop, x))]]
        (Nop, x) -> [Program $ V.update prog [(i, (Jmp, x))]]
        _        -> []

main :: IO ()
main = do

    Right program <- parse parser <$> readFile "day08/input"

    print $ part1 program

    print $ part2 program

    where
    part1 program = run program 
                  $ Computer 0 0

    part2 = head
          . rights
          . map (`run` (Computer 0 0))
          . mutations
