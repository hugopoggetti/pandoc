{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-hugo.poggetti
-- File description:
-- Main
-}

module Main (main) where

import OptsParserSystem

import System.Environment (getArgs)
import System.Exit

start :: [String] -> Opts -> IO ()
start args opts
    | globalOptsChecker args opts == False =
        putStrLn usage >> exitWith(ExitFailure 84)
    | otherwise = return ()

main :: IO ()
main = do
    args <- getArgs
    start args (optsParser args)