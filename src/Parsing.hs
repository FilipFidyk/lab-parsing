module Parsing where

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Applicative.Combinators
import Data.Void
import Data.Char(isDigit)


type Parser = Parsec Void String

data Expr = Add Expr Expr | Subtract Expr Expr | Multiply Expr Expr | Parens Expr | Number Int
    deriving Show

number :: Parser Int
number = do
    x <- takeWhileP Nothing isDigit
    pure (read x)  

parseExpr :: Parser Expr
parseExpr = try parsePlus <|> try parseSub <|> try parseMul <|> try parseParens <|> parseNumber
    

parsePlus :: Parser Expr
parsePlus = do
    _ <- char '+'
    hspace 
    x <- parseExpr
    hspace
    y <- parseExpr
    pure (Add x y)

parseSub :: Parser Expr
parseSub = do
    _ <- char '-'
    hspace 
    x <- parseExpr
    hspace
    y <- parseExpr
    pure (Subtract x y)

parseMul :: Parser Expr
parseMul = do
    _ <- char '*'
    hspace 
    x <- parseExpr
    hspace
    y <- parseExpr
    pure (Multiply x y)

parseParens :: Parser Expr
parseParens = do
    _ <- char '('
    hspace
    x <- parseExpr
    hspace
    _ <- char ')'
    pure (Parens x)

parseNumber :: Parser Expr
parseNumber = do
    x <- number
    pure (Number x)


eval :: Expr -> Int
eval (Add x y) = eval x + eval y
eval (Subtract x y) = eval x - eval y
eval (Multiply x y) = eval x * eval y
eval (Parens x) = eval x
eval (Number x) = x


{- Your code goes here! -}
{- The lab sheet contains all the instructions :) -}