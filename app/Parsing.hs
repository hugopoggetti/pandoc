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

parsefile :: String -> Opts -> IO ()
parsefile content opts
    | fromJust(outputFormat opts) == "json" = putStrLn (jsonRender (parseJson content newdoc))
    | fromJust(outputFormat opts) == "markdown" = putStrLn (markdownRender (parsemd content newdoc))
    | fromJust(outputFormat opts) == "xml" = putStrLn (xmlRender (parsexml content newdoc))
    | otherwise = putStrLn "ther's error" >> exitWith (ExitFailure 84)

--   -
--   {}
--   <@name> </@name>