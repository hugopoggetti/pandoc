{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-hugo.poggetti
-- File description:
-- Main
-}

module Parsing (parsefile) where

import OptsParserSystem
import Ast.Document


parsefile :: String -> Opts -> ?
parsefile content data
    | (inputFormat data) == "json" = 
    | (inputFormat data) == "markdown" = 
    | (inputFormat data) == "xml" = 