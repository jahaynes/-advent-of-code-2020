import Common.Parser
import Common.Parser.Combinator
import Common.Parser.String

import           Control.Applicative ((<|>))
import           Data.Char           (isSpace)
import           Data.Functor        ((<&>))

import Prelude hiding (dropWhile)

_sampleInput :: String
_sampleInput = "1 + 2 * 3 + 4 * 5 + 6"

{- Lex the input string into Tokens -}
data Token = TInt !Int
           | TPlus
           | TMul
           | TLParen
           | TRParen
               deriving (Eq, Show)

lexer :: Parser String [Token]
lexer = dropWhile isSpace *> many1 (token <* dropWhile isSpace)
    where
    token = TInt    <$> int
        <|> TPlus   <$  char '+'
        <|> TMul    <$  char '*'
        <|> TLParen <$  char '('
        <|> TRParen <$  char ')'

flip' :: [Token] -> [Token]
flip' = go []
    where
    go acc [] = acc
    go acc (TLParen:ts) = go (TRParen:acc) ts
    go acc (TRParen:ts) = go (TLParen:acc) ts
    go acc (t:ts)       = go (t:acc) ts

data Expr = Mul !Expr !Expr
          | Sum !Expr !Expr
          | Num !Int

{- Parse the Tokens into Expressions -}

-- '+' and '*' equal precedence
part1Parser :: Parser [Token] Expr
part1Parser = productTerm <|> sumTerm <|> parenExpr part1Parser
    where
    productTerm = do
        a <- parenExpr part1Parser
        _ <- such (== TMul)
        b <- part1Parser
        pure $ Mul a b
    sumTerm = do
        a <- parenExpr part1Parser
        _ <- such (== TPlus)
        b <- part1Parser
        pure $ Sum a b

-- '+' binds tighter than '*'
part2Parser :: Parser [Token] Expr
part2Parser = productTerm

    where
    productTerm = productTerm' <|> sumTerm
        where
        productTerm' = do
            a <- sumTerm
            _ <- such (==TMul)
            b <- productTerm
            pure $ Mul a b

    sumTerm = sumTerm' <|> parenExpr part2Parser
        where
        sumTerm' = do
            a <- parenExpr part2Parser
            _ <- such (==TPlus)
            b <- sumTerm
            pure $ Sum a b

parenExpr :: Parser [Token] Expr -> Parser [Token] Expr
parenExpr wow = such (==TLParen) *> wow <* such (==TRParen)
            <|> term

    where
    term :: Parser [Token] Expr
    term = one <&> \(TInt i) -> Num i

eval :: Expr -> Int
eval (Mul a b) = eval a * eval b
eval (Sum a b) = eval a + eval b
eval   (Num n) = n

main :: IO ()
main = do

    inputs <- lines <$> readFile "./day18/input"

    print $ part1 inputs
    print $ part2 inputs

    where
    part1 = sum . map evalLine
        where
        evalLine input =
            let Right expr = parse part1Parser =<< (flip' <$> parse lexer input)
            in eval expr

    part2 = sum . map evalLine
        where
        evalLine input =
            let Right expr = parse part2Parser =<< (flip' <$> parse lexer input)
            in eval expr

            -- 55699621957369