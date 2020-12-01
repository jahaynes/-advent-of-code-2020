module Common.Parser.Combinator where

import Common.Parser (Parser (..))

many :: Parser s a -> Parser s [a]
many p = Parser (go [])
  where
  go acc s =
    case runParser p s of
      Left _ -> Right (s, reverse acc)
      Right (s', a) -> go (a:acc) s'

many1 :: Parser s a -> Parser s [a]
many1 p = do
  xs <- many p
  if null xs
    then fail "none found for many1"
    else pure xs

list :: Eq a => [a] -> Parser [a] [a]
list xs = Parser $ \s ->
  let len = length xs
  in
  if xs == take len s
    then Right (drop len s, xs)
    else Left "string mismatch"

remainder :: Parser [a] [a]
remainder = Parser $ \s -> Right ([], s)

eof :: Parser [a] ()
eof = Parser go
  where
  go [] = Right ([], ())
  go  _ = Left "not eof"
