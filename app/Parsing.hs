{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-hugo.poggetti
-- File description:
-- Main
-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Parsing (parsefile) where

import OptsParserSystem
import Ast.Document
import Data.Maybe
import Unformatter.Unformatjson
import Renders.MarkdownRender.MarkdownRender (markdownRender)
import Renders.JsonRender.JsonRender (jsonRender)
import Renders.XmlRender.XmlRender (xmlRender)
import System.Exit
import Unformatter.Unformatxml (parsexml)
import Unformatter.Unformatmd (parsemd)

getinputfile :: String -> Opts -> Document
getinputfile content opts
    | fromJust (inputFormat opts) == "json" = parseJson content newdoc
    | fromJust (inputFormat opts) == "markdown" = parsemd content newdoc
    | fromJust (inputFormat opts) == "xml" = parsexml content newdoc
    | otherwise = newdoc


parsefile :: String -> Opts -> IO ()
parsefile content opts
    | fromJust (outputFormat opts) == "json" = putStrLn (jsonRender (getinputfile content opts))
    | fromJust (outputFormat opts) == "markdown" = putStrLn (markdownRender (getinputfile content opts))
    | fromJust (outputFormat opts) == "xml" = putStrLn (xmlRender (getinputfile content opts))
    | otherwise = putStrLn "ther's error" >> exitWith (ExitFailure 84)

--   -
--   {}
--   <@name> </@name>