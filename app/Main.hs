{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-hugo.poggetti
-- File description:
-- Main
-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

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
import OptsParserSystem
import System.Environment (getArgs)
import System.Exit
import System.IO
import Text.Read (get)
import Ast.Document
import Data.Maybe (fromJust, isNothing)
import Parsing
--import Debug.Trace

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn delimiter s = case break (== delimiter) s of
    (h, []) -> [h]
    (h, t)  -> h : splitOn delimiter (drop 1 t)

getFileExtension :: String -> String
getFileExtension fname = last (splitOn '.' fname)

getFileBaseName :: String -> String
getFileBaseName fname = head (splitOn '.' fname)

getOutputFormat :: String -> Maybe String
getOutputFormat "markdown" = Just "md"
getOutputFormat "xml" = Just "xml"
getOutputFormat "json" = Just "json"
getOutputFormat _ = Nothing

isValidFile :: Opts -> Opts
isValidFile opts
    | fromJust (inputFormat opts) == "markdown" = opts
    | fromJust (inputFormat opts) == "xml" = opts
    | fromJust (inputFormat opts) == "json" = opts
    | otherwise = opts {inputFormat = Nothing}

checkoptionnal :: Opts -> Opts
checkoptionnal opts
  | inputFormat opts == Nothing =
      let ext = getFileExtension (fromJust (inputFile opts))
          fmt = if ext == "md" then "markdown" else ext
      in checkoptionnal opts { inputFormat = Just fmt }
  | outputFile opts == Nothing =
      let name = getFileBaseName (fromJust (inputFile opts))
      in checkoptionnal opts { outputFile = Just ("./" ++ name ++ "."
      ++ fromJust (getOutputFormat (fromJust (outputFormat opts)))) }
  | otherwise = isValidFile opts

readthefile :: String -> IO String
readthefile path = do
    fileHandle <- openFile path ReadMode
    hGetContents fileHandle

validInput :: Opts -> Bool
validInput opts = if fromJust (inputFormat opts) == getFileExtension
    (fromJust (inputFile opts)) then True else False

start :: [String] -> Opts -> IO ()
start args opts
    | not (globalOptsChecker args opts) =
        putStrLn usage >> exitWith (ExitFailure 84)
    | otherwise = do
        filecontent <- readthefile (fromJust (inputFile opts))
        if isNothing (inputFormat opts) || (validInput opts) == False then
            putStrLn usage >> exitWith (ExitFailure 84) else parsefile 
            filecontent newdoc (checkoptionnal opts)

main :: IO ()
main = do
    args <- getArgs
    start args (optsParser args)
