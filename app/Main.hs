module Main (main) where

import Lib (Parser, runParser,
        parseChar,
        parseUInt,
        parseInt,
        parseTuple,
        parseAnyChar,
        parseOr,
        parseAnd,
        parseAndWith,
        parseMany,
        parseSome)
import Control.Applicative ((<|>), (<*>))

main :: IO ()
main = do
    input <- getLine
    let parser = parseTuple parseInt
    case runParser parser input of
        Just (result, rest) -> putStrLn $ "Parsed character: " ++ show result ++ ", remaining string: " ++ rest
        Nothing -> putStrLn "Failed to parse input"
