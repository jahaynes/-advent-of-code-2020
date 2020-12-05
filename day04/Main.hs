import Common.Parser
import Common.Parser.Combinator
import Common.Parser.String as P

import           Control.Applicative ((<|>))
import           Control.Monad       (unless)
import           Data.Char           (isDigit, isSpace)
import           Data.List           (groupBy)
import           Data.Maybe          (fromJust, isJust)
import           Data.Map      (Map)
import qualified Data.Map as M
import           Data.Set      (Set)
import qualified Data.Set as S
import           Text.Read           (readMaybe)

_sampleInput :: String
_sampleInput = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
               \byr:1937 iyr:2017 cid:147 hgt:183cm\n\
               \\n\
               \iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n\
               \hcl:#cfa07d byr:1929\n\
               \\n\
               \hcl:#ae17e1 iyr:2013\n\
               \eyr:2024\n\
               \ecl:brn pid:760753108 byr:1931\n\
               \hgt:179cm\n\
               \\n\
               \hcl:#cfa07d eyr:2025 pid:166559648\n\
               \iyr:2011 ecl:brn hgt:59in"

required :: Set String
required = S.fromList [ "byr", "iyr", "eyr"
                      , "hgt", "hcl", "ecl"
                      , "pid" ]

validEcls :: Set String
validEcls = S.fromList ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

newtype Info =
    Info (Map String String)
        deriving Show

data Passport = Passport (Map String String)
              | NorthPoleCredentials (Map String String)
              | Invalid
                deriving (Eq, Show)

isCredential :: Passport -> Bool
isCredential (Passport _) = True
isCredential (NorthPoleCredentials _) = True
isCredential _ = False

parser2 :: Parser String [Info]
parser2 = map (Info . M.fromList . map fromJust)
        . filter (isJust . head)
        . groupBy (\a b -> null a == null b)
      <$> many (new <|> cont)
    where
    new :: Parser String (Maybe a)
    new = const Nothing <$> char_ '\n'

    cont :: Parser String (Maybe (String, String))
    cont = do
        k <- takeUntil (==':')
        char_ ':'
        v <- P.takeWhile (not . isSpace)
        char_ '\n' <|> char_ ' ' <|> ok
        pure $ Just (k, v)

validate :: Passport -> Bool
validate (Passport m)             = validateMap (Info m)
validate (NorthPoleCredentials m) = validateMap (Info m)
validate _                        = False

validateMap :: Info -> Bool
validateMap (Info m) = all isJust [ validByr, validIyr, validEyr
                                  , validHgt, validHcl, validEcl
                                  , validPid ]

    where
    validByr = M.lookup "byr" m
           >>= readMaybe
           >>= between 1920 2002

    validIyr = M.lookup "iyr" m
           >>= readMaybe
           >>= between 2010 2020

    validEyr = M.lookup "eyr" m
           >>= readMaybe
           >>= between 2020 2030

    validHgt = M.lookup "hgt" m
           >>= parseMaybe (validCm <|> validIn)
        where
        validCm = between 150 193 =<< int <* string "cm"
        validIn = between  59  76 =<< int <* string "in"

    validHcl = M.lookup "hcl" m
           >>= parseMaybe hcl
        where
        hcl = do char_ '#'
                 many_ (such (\c -> isDigit c || c >= 'a' && c <= 'f'))

    validEcl = do col <- M.lookup "ecl" m
                  assert $ col `S.member` validEcls

    validPid = M.lookup "pid" m
           >>= parseMaybe validCid
        where
        validCid = do ds <- many digit
                      unless (length ds == 9) (fail "")

    assert :: MonadFail m => Bool -> m ()
    assert b | b         = pure ()
             | otherwise = fail ""

    between :: MonadFail m => Int -> Int -> Int -> m ()
    between lo hi x =
        assert  $ x >= lo
               && x <= hi

classify :: Info -> Passport
classify (Info m) =
    let keys = M.keysSet m
    in
    if required `S.isSubsetOf` keys
        then
            if "cid" `S.member` keys
                then Passport m
                else NorthPoleCredentials m
        else Invalid

main :: IO ()
main = do

    Right input <- parse parser2 <$> readFile "day04/input"

    part1 input

    part2 input

    where
    part1 = print
          . length
          . filter isCredential
          . map classify

    part2 = print
          . length
          . filter validate
          . map classify
