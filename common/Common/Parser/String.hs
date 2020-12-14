module Common.Parser.String where

import Common.Parser            (Parser (..))
import Common.Parser.Combinator

import Control.Applicative ((<|>))
import Data.Char           (digitToInt, isDigit, isSpace)
import Data.Functor        ((<&>), void)

import Prelude hiding (takeWhile, dropWhile)

char :: Char -> Parser String Char
char c = such (==c)

char_ :: Char -> Parser String ()
char_ c = void $ such (==c)

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

whitespace :: Parser String String
whitespace = takeWhile isSpace

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
      
string_ :: String -> Parser String ()
string_ = void <$> string

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
  
end :: Parser String ()
end = endOfLine <|> eof
