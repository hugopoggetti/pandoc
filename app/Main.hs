{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-hugo.poggetti
-- File description:
-- Main
-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main (main) where

import Renders.MarkdownRender.MarkdownRender (markdownRender)
import Renders.JsonRender.JsonRender (jsonRender)
import Renders.XmlRender.XmlRender (xmlRender)
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
import Utils
import Parsing
import Debug.Trace


getFileExtension :: String -> String
getFileExtension fname = if last (splitOn '.' fname) == "md" then
    "markdown" else last (splitOn '.' fname)

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
  | otherwise = isValidFile opts

readthefile :: String -> IO String
readthefile path = do
    fileHandle <- openFile path ReadMode
    hGetContents fileHandle

ismd :: String -> String
ismd format = if format == "md" then "markdown" else format

validInput :: Opts -> Bool
validInput opts
    | inputFormat opts == Nothing = validInput (opts {inputFormat =
        Just(getFileExtension (fromJust (inputFile opts)))})
    | otherwise = if fromJust (inputFormat opts) ==  ismd (getFileExtension(fromJust(inputFile opts)))
        then True else False

start :: [String] -> Opts -> IO ()
start args opts
    | not (globalOptsChecker args opts) =
        putStrLn usage >> exitWith (ExitFailure 84)
    | otherwise = do
        filecontent <- readthefile (fromJust (inputFile opts))
        if (validInput opts) == False then 
            putStrLn usage >> exitWith (ExitFailure 84) else parsefile 
            filecontent (checkoptionnal opts)

main :: IO ()
main = do
    args <- getArgs
    start args (optsParser args)

