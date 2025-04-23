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
import Data.Maybe (fromJust)
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

checkoptionnal :: Opts -> Opts
checkoptionnal opts
  | inputFormat opts == Nothing =
      let ext = getFileExtension (fromJust(inputFile opts))
          fmt = if ext == "md" then "markdown" else ext
      in checkoptionnal opts { inputFormat = Just fmt }
  | outputFile opts == Nothing =
      let name = getFileBaseName (fromJust(inputFile opts))
          out  = "./" ++ name ++ "." ++ fromJust(outputFormat opts)
      in checkoptionnal opts { outputFile = Just out }
  | otherwise = opts


-- parsefile
 

start :: [String] -> Opts -> IO ()
start args opts
    | not (globalOptsChecker args opts) =
        putStrLn usage >> exitWith (ExitFailure 84)
    | otherwise = return ()--parsefile
        -- do 
        -- putStrLn ((optsToList opts)!!0)
        -- putStrLn ((optsToList opts)!!1)
        -- putStrLn ((optsToList opts)!!2)
        -- putStrLn ((optsToList opts)!!3)
        -- putStrLn ((optsToList opts)!!4)
        -- putStrLn ((optsToList opts)!!5)
        -- putStrLn ((optsToList opts)!!6)
        -- putStrLn ((optsToList opts)!!7)


readthefile :: String -> IO String
readthefile path = do
    fileHandle <- openFile path ReadMode
    hGetContents fileHandle

main :: IO ()
main = do
    args <- getArgs
    start args (optsParser args)
