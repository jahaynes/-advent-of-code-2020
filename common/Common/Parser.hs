module Common.Parser where

import Control.Applicative

newtype Parser s a =
  Parser { runParser :: s -> Either String (s, a) }

instance Functor (Parser s) where

  fmap f (Parser run) = Parser $ \s ->
    case run s of
      Left l        -> Left l
      Right (s', x) -> Right (s', f x)

instance Applicative (Parser s) where

  pure a = Parser $ \s -> Right (s, a)

  pf <*> px = Parser $ \s ->
    case runParser pf s of
      Left l -> Left l
      Right (s', f) ->
        case runParser px s' of
          Left l -> Left l
          Right (s'', x) -> Right (s'', f x)

instance Alternative (Parser s) where

  empty = Parser $ \_ -> Left "no more alternatives"

  p <|> q = Parser $ \s ->
    case runParser p s of
      Right r -> Right r
      Left  _ ->
        case runParser q s of
          Right r -> Right r
          Left  _ -> Left "no matches"

instance Monad (Parser s) where

  return = pure

  px >>= pf = Parser $ \s ->
    case runParser px s of
      Left l -> Left l
      Right (s', x) ->
        let Parser r = pf x
        in r s'

instance MonadFail (Parser s) where
  fail s = Parser $ \_ -> Left s

parse :: (Show (f s), Foldable f) => Parser (f s) a -> f s -> Either String a
parse p s =
  case runParser p s of
    Left l                    -> Left l
    Right (s', x) | null s'   -> Right x
                  | otherwise -> Left $ "Leftover input: " ++ take 50 (show s') ++ "..."
