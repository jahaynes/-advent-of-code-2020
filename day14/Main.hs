import Common.Parser
import Common.Parser.Combinator
import Common.Parser.String

import           Control.Applicative       ((<|>))
import           Data.Bits                 (clearBit, setBit, testBit)
import           Data.List                 (foldl')
import           Data.IntMap               (IntMap)
import qualified Data.IntMap as M
import           Data.Vector.Unboxed       (Vector)
import qualified Data.Vector.Unboxed as VU

_sampleInput1 :: String
_sampleInput1 = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n\
                \mem[8] = 11\n\
                \mem[7] = 101\n\
                \mem[8] = 0"

_sampleInput2 :: String
_sampleInput2 = "mask = 000000000000000000000000000000X1001X\n\
                \mem[42] = 100\n\
                \mask = 00000000000000000000000000000000X0XX\n\
                \mem[26] = 1"

data Store =
    Store { getMask :: !(Vector (Int, Char))
          , getRam  :: !(IntMap Int)
          } deriving Show

newStore :: Store
newStore = Store { getMask = mempty
                 , getRam  = mempty
                 }

data Instruction = SetMask !(Vector Char)

                 | SetMem { getAddr :: !Int
                          , getVal  :: !Int
                          } deriving Show

data V = V1 | V2

apply :: V -> Store -> Instruction -> Store

apply _ store (SetMask mask) =
    store { getMask = VU.imap (,) $ VU.reverse mask }

apply V1 store (SetMem addr val) =
    store { getRam = M.insert addr (applyMaskToValue val $ getMask store) $ getRam store }

    where
    applyMaskToValue :: Int -> Vector (Int, Char) -> Int
    applyMaskToValue = VU.foldl' f
        where
        f v (b, '0') = clearBit v b
        f v (b, '1') = setBit   v b
        f v        _ =          v

apply V2 store (SetMem addr val) =
    store { getRam = foldr (`M.insert` val) (getRam store) . applyMaskToAddress (getMask store) $ addr }

    where
    applyMaskToAddress :: Vector (Int, Char) -> Int -> [Int]
    applyMaskToAddress mask = map fromVec
                            . branch
                            . zipWith f (VU.toList mask)
                            . toVec

        where
        f (_, '0') x = x
        f (_, '1') _ = '1'
        f (_, 'X') _ = 'X'
        f        _ _ = undefined

        branch :: [Char] -> [[Char]]
        branch              [] = [[]]
        branch ('X':xs) = "01" >>= \prefix -> (prefix:) <$> branch xs
        branch   (x:xs) =                          (x:) <$> branch xs

        toVec :: Int -> [Char]
        toVec = go 0
            where
            go 36 _ = []
            go  b a | testBit a b = '1' : go (b+1) a
                    | otherwise   = '0' : go (b+1) a

        fromVec :: [Char] -> Int
        fromVec = go 0 0
            where
            go a _       [] = a
            go a b ('0':xs) = go         a    (b+1) xs
            go a b ('1':xs) = go (setBit a b) (b+1) xs
            go _ _        _ = undefined

part :: V -> [Instruction] -> Int
part v = sum . M.elems . getRam . foldl' (apply v) newStore

main :: IO ()
main = do

    Right instrs <- parse parser <$> readFile "day14/input"

    print $ part V1 instrs -- 6513443633260

    print $ part V2 instrs -- 3442819875191

    where
    parser :: Parser String [Instruction]
    parser = many (mask <|> mem)
        where
        mask = SetMask . VU.fromList <$> (string "mask = " *> line)
        mem = do
            a <- string "mem[" *> int
            v <- string "] = " *> int
            SetMem a v <$ end
