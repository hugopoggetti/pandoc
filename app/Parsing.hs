{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-hugo.poggetti
-- File description:
-- Main
-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Parsing (parsefile) where

import OptsParserSystem
import Ast.Document (newdoc, Document)
import Data.Maybe
import Renders.MarkdownRender.MarkdownRender (markdownRender)
import Renders.JsonRender.JsonRender (jsonRender)
import Renders.XmlRender.XmlRender (xmlRender)
import System.Exit
import Parsers.JsonParser (parseJson)
import Parsers.XmlParser (parsexml)
import Parsers.MarkdownParser (parsemd)

getinputfile :: String -> Opts -> Maybe Document
getinputfile content opts
    | fromJust (inputFormat opts) == "json" = parseJson content newdoc
    | fromJust (inputFormat opts) == "markdown" =  parsemd content newdoc
    | fromJust (inputFormat opts) == "xml" = parsexml content newdoc
    | otherwise = Nothing

parsefile :: String -> Opts -> IO ()
parsefile content opts
    |fromJust(outputFormat opts)=="json"=let js=getinputfile content opts in if
    js==Nothing then exitWith(ExitFailure 84)else
        putStrLn(jsonRender (fromJust js))
    |fromJust(outputFormat opts)=="markdown"=let md=getinputfile content opts
    in if md==Nothing then exitWith(ExitFailure 84)
    else putStrLn(markdownRender (fromJust md))
    |fromJust(outputFormat opts)=="xml"=let xml=getinputfile content opts in
    if xml==Nothing then exitWith(ExitFailure 84)
    else putStrLn(xmlRender (fromJust xml))|otherwise=exitWith(ExitFailure 84)
