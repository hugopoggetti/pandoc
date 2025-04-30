{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-hugo.poggetti
-- File description:
-- Lib
-}

{-# LANGUAGE InstanceSigs #-}

module Lib
    ( Parser,
        runParser,
        parseChar,
        parseUInt,
        parseInt,
        parseTuple,
        parseAnyChar,
        parseOr,
        parseAnd,
        parseAndWith,
        parseMany,
        parseString,
        parseSome
    ) where

import Control.Applicative (Alternative(..))
import Text.Read (readMaybe)

data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input ->
        case p input of
            Just (result, rest) -> Just (f result, rest)
            Nothing -> Nothing

instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input)
    (Parser pf) <*> (Parser px) = Parser $ \input ->
        case pf input of
            Just (f, rest) -> case px rest of
                Just (x, rest') -> Just (f x, rest')
                Nothing -> Nothing
            Nothing -> Nothing

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input ->
        case p1 input of
            Just result -> Just result
            Nothing -> p2 input

instance Monad Parser where
  return = pure
  (Parser p) >>= f = Parser $ \input ->
    case p input of
      Just (x, rest) -> runParser (f x) rest
      Nothing -> Nothing

parseChar :: Char -> Parser Char
parseChar a = Parser $ \input -> case input of
    [] -> Nothing
    (x:xs) -> if a == x then Just (x, xs) else Nothing

parseString :: String -> Parser String
parseString str = Parser $ \input ->
    let (prefix, rest) = splitAt (length str) input
    in if prefix == str then Just (str, rest) else Nothing

parseUInt :: Parser Int
parseUInt = Parser $ \input ->
    let (digits, rest) = span (`elem` ['0'..'9']) input
    in case readMaybe digits of
        Just n -> Just (n, rest)
        Nothing -> Nothing

parseInt :: Parser Int
parseInt = Parser $ \input ->
    case runParser (parseChar '-') input of
        Just (_, rest) ->
            case runParser parseUInt rest of
                Just (n, rest') -> Just (-n, rest')
                Nothing -> Nothing
        Nothing -> runParser parseUInt input

parseTuple :: Parser a -> Parser (a, a)
parseTuple p = Parser $ \input ->
    case runParser (parseChar '(') input of
        Just (_, rest) ->
            case runParser (parseAnd p (parseChar ',')) rest of
                Just ((res1, _), rest') -> 
                    case runParser (parseAnd p (parseChar ')')) rest' of
                        Just ((res2, _), rest'') -> Just ((res1, res2), rest'')
                        Nothing -> Nothing
                Nothing -> Nothing
        Nothing -> Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar chars = Parser $ \input ->
    case input of
        [] -> Nothing
        (x:xs) -> if x `elem` chars then Just (x, xs) else Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 = Parser $ \input ->
    case runParser p1 input of
        Just result -> Just result
        Nothing -> runParser p2 input

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 = Parser $ \input ->
    case runParser p1 input of
        Just (result1, rest) -> 
            case runParser p2 rest of
                Just (result2, rest') -> Just ((result1, result2), rest')
                Nothing -> Nothing
        Nothing -> Nothing

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 = Parser $ \input ->
    case runParser (parseAnd p1 p2) input of
        Just ((result1, result2), rest) -> Just (f result1 result2, rest)
        Nothing -> Nothing

parseMany :: Parser a -> Parser [a]
parseMany p = Parser $ \input ->
    case runParser p input of
        Just (result, rest) -> 
            case runParser (parseMany p) rest of
                Just (results, rest') -> Just (result:results, rest')
                Nothing -> Just ([result], rest)
        Nothing -> Just ([], input)

parseSome :: Parser a -> Parser [a]
parseSome p = Parser $ \input ->
    case runParser p input of
        Just (result, rest) -> 
            case runParser (parseMany p) rest of
                Just (results, rest') -> Just (result:results, rest')
                Nothing -> Just ([result], rest)
        Nothing -> Nothing
