{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-hugo.poggetti
-- File description:
-- Main
-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Parsers.JsonParser (parseJson) where

import Utils
import Ast.Document (Document)

data Jsoncutter = Jsoncutter {
    stringWrap  :: String -> String,
    keyValue    :: (String, String) -> String,
    object      :: [String] -> String,
    array       :: [String] -> String,
    metaJson    :: [(String, String)] -> String,
    headerJson  :: Int -> String -> String,
    textFormat  :: String -> String -> String, -- e.g., "bold", "italic"
    linkJson    :: (String, String) -> String,
    imageJson   :: (String, String) -> String
}

jsonCutterDefault :: Jsoncutter
jsonCutterDefault = Jsoncutter {
    stringWrap = \s -> "\"" ++ s ++ "\"",
    keyValue = \(k, v) -> "\"" ++ k ++ "\":" ++ v,
    object = \kvs -> "{" ++ joinWithComma kvs ++ "}",
    array = \elems -> "[" ++ joinWithComma elems ++ "]",
    metaJson = \pairs -> "{" ++ joinWithComma (map (\(k,v) -> "\"" ++ k ++ "\":\"" ++ v ++ "\"") pairs) ++ "}",
    headerJson = \n txt -> "{\"header" ++ show n ++ "\":\"" ++ txt ++ "\"}",
    textFormat = \fmt txt -> "{\"" ++ fmt ++ "\":\"" ++ txt ++ "\"}",
    linkJson = \(text, url) -> "{\"link\":{\"text\":\"" ++ text ++ "\",\"url\":\"" ++ url ++ "\"}}",
    imageJson = \(alt, src) -> "{\"image\":{\"alt\":\"" ++ alt ++ "\",\"src\":\"" ++ src ++ "\"}}"
}

data Jsonelement = Jsonelement {
    header      :: String,
    title       :: String,
    author      :: String,
    date        :: String,
    body        :: String,
    section     :: String,
    content     :: String,
    bold        :: String,
    italic      :: String,
    code        :: String,
    codeblock   :: String
}

jsonelementdefault :: Jsonelement
jsonelementdefault = Jsonelement {
    header = "\"header\":",
    title = "\"title\":",
    author = "\"author\":",
    date = "\"date\":",
    body = "\"body\":",
    section = "\"section\":",
    content = "\"content\":",
    bold = "\"bold\":",
    italic = "\"italic\":",
    code = "\"code\":",
    codeblock = "\"codeblock\":"
}

parseJson :: String -> Document -> Document
parseJson file newdoc = newdoc
