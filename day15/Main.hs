import           Data.IntMap      (IntMap)
import qualified Data.IntMap as M

_sampleInput1 :: String
_sampleInput1 = "3,1,2"

newtype SpokenAt =
     SpokenAt (IntMap [Int]) deriving Show

newtype LastSpoken =
     LastSpoken Int deriving Show

newtype Turn =
     Turn Int deriving Eq

seed :: [Int] -> (Turn, SpokenAt, LastSpoken)
seed xs = ( Turn (length xs + 1)
          , SpokenAt . M.fromList $ zip xs (map (:[]) [1..])
          , LastSpoken $ last xs )

main :: IO ()
main = do

     input <- do input <- readFile "./day15/input"
                 pure . (read :: String -> [Int]) $ '[':input++"]"

     let (turn, spokenAt, lastSpoken) = seed input

     print $ go turn     2020 spokenAt lastSpoken
     print $ go turn 30000000 spokenAt lastSpoken

go :: Turn -> Int -> SpokenAt -> LastSpoken -> LastSpoken
go (Turn turn) target (SpokenAt spokenAt) ls@(LastSpoken lastSpoken) =
     let turn' = Turn $ turn + 1
     in if turn == target + 1
          then ls
          else
               case M.lookup lastSpoken spokenAt of

                    Just [_] ->
                       let n = 0
                           spokenAt' = M.insertWith (++) n [turn] spokenAt
                       in go turn' target (SpokenAt spokenAt') (LastSpoken n)

                    Just (x:y:_) ->
                       let n = x - y
                           spokenAt' = M.insertWith (++) n [turn]
                                     . M.insert lastSpoken [x,y]
                                     $ spokenAt
                       in go turn' target (SpokenAt spokenAt') (LastSpoken n)

                    _ -> error "nope"
