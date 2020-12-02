module Common.Parser.String where

import Common.Parser            (Parser (..))
import Common.Parser.Combinator

import Control.Applicative ((<|>))
import Data.Char           (isDigit, digitToInt)
import Data.Functor        ((<&>))

import Prelude hiding (takeWhile, dropWhile)

such :: (Char -> Bool) -> Parser String Char
such f = Parser go
  where
  go     []             = Left "out of input"
  go (x:xs) | f x       = Right (xs, x)
  go     _  | otherwise = Left "mismatch"

suchnt :: (Char -> Bool) -> Parser String Char
suchnt f = such (not . f)

char :: Char -> Parser String Char
char c = such (==c)

oneChar :: Parser String Char
oneChar = Parser f
  where
  f     [] = Left "no char available"
  f (x:xs) = Right (xs, x)

digit :: Parser String Int
digit = Parser $ \s -> runParser (such isDigit) s <&> (\(s', x) -> (s', digitToInt x))

nat :: Parser String Int
nat = fst . foldr (\d (a, b) -> (a + d * b, b * 10)) (0, 1) <$> many1 digit

int :: Parser String Int
int = nat
  <|> negate <$> (char '-' *> nat)

takeWhile :: (Char -> Bool) -> Parser String String
takeWhile p = many (such p)

takeWhile1 :: (Char -> Bool) -> Parser String String
takeWhile1 p = many1 (such p)

dropWhile :: (Char -> Bool) -> Parser String ()
dropWhile p = many (such p) <&> \_ -> ()

dropWhile1 :: (Char -> Bool) -> Parser String ()
dropWhile1 p = many1 (such p) <&> \_ -> ()

takeUntil :: (a -> Bool) -> Parser [a] [a]
takeUntil p = Parser (go [])
  where
  go   _     []             = Left "out of input"
  go acc (x:xs) | p x       = Right (x:xs, reverse acc)
                | otherwise = go (x:acc) xs

takeUntil' :: (a -> Bool) -> Parser [a] [a]
takeUntil' p = Parser (go [])
  where
  go   _     []             = Left "out of input"
  go acc (x:xs) | p x       = Right (xs, reverse (x:acc))
                | otherwise = go (x:acc) xs

string :: String -> Parser String String
string = list
      
endOfLine :: Parser String ()
endOfLine = (string "\r\n" <&> \_ -> ())
        <|> (char   '\n'   <&> \_ -> ())
        <|> (char   '\r'   <&> \_ -> ())

line :: Parser String String
line = do
  ln <- takeWhile (\x -> x /= '\n' && x /= '\r')
  done <- ((\_ -> False) <$> endOfLine)
      <|> ((\_ ->  True) <$> eof)
  if null ln && done
    then fail "eof"
    else pure ln
  

