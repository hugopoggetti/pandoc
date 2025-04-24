{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-hugo.poggetti
-- File description:
-- Main
-}

module Parsing (parsefile) where

import OptsParserSystem
import Ast.Document
import Data.Maybe


parsefile :: String -> Opts -> IO ()
parsefile content opts = do
    putStrLn (fromJust (inputFormat opts))

    -- | (inputFormat opts) == "json" = 
    -- | (inputFormat opts) == "markdown" = 
    -- | (inputFormat opts) == "xml" = 