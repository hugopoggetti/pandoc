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
import Renders.HtmlRender.HtmlRender (htmlRender)
import Renders.DebugRender.DebugRender (debugRender)
import System.Exit
import Parsers.JsonParser (parseJson)
import Parsers.XmlParser (parsexml)
import Parsers.MarkdownParser (parsemd)

-- | return list of supported output format
getSupportedOutput :: [String]
getSupportedOutput = ["html", "json", "markdown", "xml", "natif"]

getinputfile :: String -> Opts -> Maybe Document
getinputfile content opts
    | fromJust (inputFormat opts) == "json" = parseJson content newdoc
    | fromJust (inputFormat opts) == "markdown" =  parsemd content newdoc
    | fromJust (inputFormat opts) == "xml" = parsexml content newdoc
    | otherwise = Nothing


-- | return string of the following format
getValidDoc :: String -> Document -> String
getValidDoc "html" doc = htmlRender doc
getValidDoc "xml" doc = xmlRender doc
getValidDoc "json" doc = jsonRender doc
getValidDoc "markdown" doc = markdownRender doc
getValidDoc "natif" doc = debugRender doc
getValidDoc _ doc = htmlRender doc

writeDocument :: Opts -> String -> IO ()
writeDocument args doc =
    case outputFile args of
        Just path -> writeFile path doc
        Nothing   -> putStrLn doc

parsefile :: String -> Opts -> IO ()
parsefile content opts 
    | fromJust (outputFormat opts) `elem` getSupportedOutput =
        let doc = getinputfile content opts
        in if doc == Nothing then exitWith(ExitFailure 84) 
        else writeDocument opts $ 
            getValidDoc (fromJust (outputFormat opts)) (fromJust doc)  
    | otherwise = exitWith(ExitFailure(84))

