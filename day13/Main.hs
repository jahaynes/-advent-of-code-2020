
import Common.Parser
import Common.Parser.Combinator
import Common.Parser.String

import Control.Applicative ((<|>))
import Data.Maybe (catMaybes, isJust)

import Prelude hiding (dropWhile)

_sampleInput :: String
_sampleInput = "939\n\
               \7,13,x,x,59,x,31,19"

newtype Bus =
    Bus Integer deriving (Eq, Ord, Show)

data Timetable =
    Timetable { _getDepartAfter :: !Integer
              , _getBuses       :: ![Maybe Bus]
              }

parser :: Parser String Timetable
parser =  do
    departAfter <- fromIntegral <$> int <* endOfLine
    buses       <- (:) <$> mBus <*> many (char ',' *> mBus)
    _           <- remainder
    pure $ Timetable departAfter buses
    where
    mBus = Nothing    <$  char 'x'
       <|> Just . Bus . fromIntegral <$> int

part1 :: Timetable -> Integer
part1 (Timetable departAfter mBuses) =
    let (soonest, Bus bus) = minimum $ map nextDeparture (catMaybes mBuses)
        wait = soonest - departAfter
    in bus * wait
    where
    nextDeparture :: Bus -> (Integer, Bus)
    nextDeparture bus@(Bus b) = (b + departAfter - (departAfter `mod` b), bus)

part2 :: [Maybe Bus] -> Integer
part2 buses =

    let offsetsBuses = map (\(o, Just (Bus b)) -> (o, b))
                     . filter (isJust . snd)
                     . zip [0..]
                     $ buses

        (x, y) = crt offsetsBuses

    in y - x

    where
    crt = foldr go (0, 1)
        where
        go (r1, m1) (r2, m2) = (r `mod` m, m)
            where
            r = r2 + m2 * (r1 - r2) * (m2 `inv` m1)
            m = m2 * m1

        -- Modular Inverse
        a `inv` m = let (_, i, _) = extGcd a m in i `mod` m

        -- Extended Euclidean Algorithm
        extGcd 0 b = (b, 0, 1)
        extGcd a b = (g, t - (b `div` a) * s, s)
            where (g, s, t) = extGcd (b `mod` a) a

main :: IO ()
main = do

    Right input <- parse parser <$> readFile "day13/input"

    print $ part1 input

    print $ part2 (_getBuses input)
