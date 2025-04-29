{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-hugo.poggetti
-- File description:
-- Main
-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Unformatter.Unformatmd (parsemd) where

import Ast.Document (Document(..), Meta(..), Inline(..))
import Lib (Parser, runParser, parseChar, parseString, parseOr, parseAnd, parseMany, parseSome)
import Debug.Trace
import Utils (splitOne, joinWithComma)

data Mdcutter = Mdcutter {
    mdmeta       :: String,
    mTitle     :: String,
    mAuthor    :: String,
    mDate      :: String,
    --header     :: Int -> String,
    bold       :: String -> String,
    italic     :: String -> String,
    code       :: String -> String,
    codeblock  :: String -> String,
    list       :: String -> String,
    link       :: (String, String) -> String,
    image      :: (String, String) -> String
}

mdcutterDefault :: Mdcutter
mdcutterDefault = Mdcutter {
    mdmeta = "---",
    mTitle = "title:",
    mAuthor = "author:",
    mDate = "date:",
    --mheader = "#header",
    bold = \text -> "**" ++ text ++ "**",
    italic = \text -> "*" ++ text ++ "*",
    code = \text -> "`" ++ text ++ "`",
    codeblock = \text -> "```" ++ text ++ "```",
    list = \text -> "- " ++ text,
    link = \(text, url) -> "[" ++ text ++ "](" ++ url ++ ")",
    image = \(alt, src) -> "![" ++ alt ++ "](" ++ src ++ ")"
}


parsemd :: String -> Document -> Document
parsemd file doc =
    let (mdmeta, rest) = case runParser (parseString "---\n") file of
            Just (res, rest) -> (res, rest)
            Nothing -> error "Failed to parse metadata"
        (title, rest2) = parseTitle rest
        (author, rest3) = parseAuthor rest2
        (date, _) = parseDate rest3
        newMeta = Meta {
            metaTitle = [Str title],
            metaAuthors = [[Str author]],
            metaDate = [Str date]
        }
    in Document newMeta (case doc of Document _ blocks -> blocks)


parseTitle :: String -> (String , String)
parseTitle rest = 
    case runParser (parseString "title:") rest of
        Just (res, rest) ->
            let suce = splitOne '\n' rest
            in (head suce, last suce)
        Nothing -> error "Failed to parse title"


parseAuthor :: String -> (String , String)
parseAuthor rest = 
    --trace rest $
    case runParser (parseString "author:") rest of
        Just (res, rest) ->
            let suce = splitOne '\n' rest
             in (head suce, last suce)
        Nothing -> error "Failed to parse author"

parseDate :: String -> (String , String)
parseDate rest = 
    case runParser (parseString "date:") rest of
        Just (res, rest) ->
            let suce = splitOne '\n' rest
            in (head suce, last suce)
        Nothing -> error "Failed to parse date"


