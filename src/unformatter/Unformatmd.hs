{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-hugo.poggetti
-- File description:
-- Main
-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Unformatter.Unformatmd () where

import Ast.Document
import Data.Maybe
import Utils

data Mdcutter = Mdcutter {
    meta       :: String,
    mTitle     :: String,
    mAuthor    :: String,
    mDate      :: String,
    header     :: Int -> String,
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
    meta = "---",
    mTitle = "title:",
    mAuthor = "author:",
    mDate = "date:",
    header = \n -> if n >= 1 && n <= 6 then replicate n '#' else error "Invalid header level",
    bold = \txt -> "**" ++ txt ++ "**",
    italic = \txt -> "*" ++ txt ++ "*",
    code = \txt -> "`" ++ txt ++ "`",
    codeblock = \txt -> "```" ++ "\n" ++ txt ++ "\n```",
    list = \item -> "- " ++ item,
    link = \(text, url) -> "[" ++ text ++ "](" ++ url ++ ")",
    image = \(alt, src) -> "![" ++ alt ++ "](" ++ src ++ ")"
}