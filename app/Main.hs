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

-- exampleDoc :: Document
-- exampleDoc = Document
--   (Meta
--     [Str "Syntaxe JSON"]
--     [[Str "Fornes Leo"]]
--     [Str "2024-01-01"]
--   )
--   [ Para [Str "This document is a simple example of the JSON syntax."]
--   , Para [Str "Every syntax element is displayed in this document."]
--   , Section 1
--       [Str "header 1"] 
--       [ Para [Str "This is a basic paragraph with text."]
--       , Para
--           [ Str "This is a paragraph with "
--           , Strong [Str "bold"]
--           , Str ", "
--           , Emph [Str "italic"]
--           , Str " and "
--           , Code "code"
--           , Str " text."
--           ]
--       , Section 2
--           [Str "header 2"]
--           [ CodeBlock "This is a code block."
--           , BulletList
--               [ [Para [Str "list item 1"]]
--               , [Para [Str "list item 2"]]
--               , [Para [Str "list item 3"]]
--               ]
--           , Para
--               [ Str "This is a paragraph with a "
--               , Link [Str "link"]
--                   ("https://www.youtube.com/watch?v=dQw4w9WgXcQ&ab_channel=RickAstley", "")
--               , Str "."
--               ]
--           , Para
--               [ Str "This is a paragraph with an image "
--               , Image [Str "Text to replace image"]
--                   ("https://cdn-images-1.medium.com/max/697/1*tsHrUKwQXG1YZX0l957ISw.png", "")
--               , Str "."
--               ]
--           , Section 3
--               []
--               [ Section 4
--                   [Str "header 4"]
--                   [ Para [Str "Every syntax element can be use separately or combined."]
--                   , Para [Str "Think about the different possible combinations."]
--                   , Para [Str "All combined syntax elements aren't displayed in this document."]
--                   ]
--               ]
--           ]
--       ]
--   ]

-- main :: IO ()
-- main = do
--     putStrLn $ replicate 50 '-'
--     putStrLn "Markdown"
--     putStrLn $ replicate 50 '-'
--     putStrLn $ markdownRender exampleDoc
--     putStrLn $ replicate 50 '-'
--     putStrLn "JSON"
--     putStrLn $ replicate 50 '-'
--     putStrLn $ jsonRender exampleDoc
--     putStrLn $ replicate 50 '-'
--     putStrLn "XML"
--     putStrLn $ replicate 50 '-'
--     putStrLn $ xmlRender exampleDoc
--     putStrLn $ replicate 50 '-'
--     putStrLn "DEBUG"
--     putStrLn $ replicate 50 '-'
--     putStrLn $ debugRender exampleDoc

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
validInput opts
    | inputFormat opts == Nothing = validInput (opts {inputFormat =
        Just(getFileExtension (fromJust (inputFile opts)))})
    | otherwise = if fromJust (inputFormat opts) == getFileExtension
    (fromJust (inputFile opts)) then True else False

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
