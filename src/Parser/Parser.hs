{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE FlexibleInstances #-}
module Parser.Parser where

import Data.Char (ord, isAlphaNum, isDigit, isAlpha, isLower, isUpper)
import Data.Functor (($>))
import Control.Monad
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class(lift)
import Control.Applicative (Alternative, Applicative, empty, (<|>))

{-
Monadic parser combinators.
Heavily inspired by the paper "Monadic Parser Combinators" with authors:
Graham Hutton - University of Nottingham
Erik Meijer - University of Utrecht
-}

instance Alternative m => Alternative (Either (m a)) where
    empty = Left empty
    (Left a) <|> (Left b) = Left (a <|> b)
    (Left a) <|> b = b
    b <|> (Left a) = b
    a <|> b = a

instance MonadPlus m => MonadPlus (Either (m a))

type ErrorMonad = Either [String]

type Parser a = StateT String ErrorMonad a

(<@>) = runStateT

pFail :: String -> Parser a
pFail err = StateT(\_ -> Left [err])

item :: Parser Char
item = get >>= \l -> item' l
  where item' [] = pFail "Unexpected EOF."
        item' (x:xs) = put xs >> return x

eof :: Parser ()
eof = get >>= \l -> eof' l
  where eof' [] = return ()
        eof' s = pFail ("Expected EOF. Got '" ++ s ++ "'")

sat :: (Char -> Bool) -> Parser Char
sat pr = [x | x <- item, pr x]

many :: Parser a -> Parser [a]
many p = [x:xs | x <- p, xs <- many p] <|> return []

skipTo :: Parser a -> Parser ()
skipTo end = [() | _ <- end] <|> [() | _ <- item, _ <- skipTo end]

many1 :: Parser a -> Parser [a]
many1 p = [x:xs | x <- p, xs <- many p]

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = [x:xs | x <- p, xs <- many [y | _ <- sep, y <- p]] <|> return []

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = [x:xs | x <- p, xs <- many [y | _ <- sep, y <- p]]

spaces :: Parser ()
spaces = [() | _ <- sat (`elem` spaces)]
  where spaces = " \t\n"

token :: Parser a -> Parser a
token p = [x | x <- p, _ <- many spaces]

alphaNumeric :: Parser Char
alphaNumeric = sat isAlphaNum

alpha :: Parser Char
alpha = sat isAlpha

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

digit :: Parser Char
digit = sat isDigit

number :: Parser Int
number = many1 digit >>= \digits -> return (read digits)

integer :: Parser Int
integer = token number

char :: Char -> Parser Char
char c = sat (== c)

bracket :: Parser o -> Parser a -> Parser c -> Parser a
bracket open p close = [x | _ <- open, x <- p, _ <- close]

string :: String -> Parser String
string "" = return ""
string s@(x:xs) = [ s | _ <- char x, _ <- string xs]

symbol :: String -> Parser String
symbol s = token $ string s <|> pFail ("Expected symbol: '" ++ s ++ "'.")

op :: Parser o -> Parser a -> Parser b -> Parser (a, o, b)
op po pa pb = [(lhs, oper, rhs) | lhs <- pa, oper <- po, rhs <- pb]

parse :: Parser a -> Parser a
parse p = [x | _ <- many spaces, x <- p, _ <- eof]

eval :: Parser a -> String -> ErrorMonad a
eval = evalStateT
