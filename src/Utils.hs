{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-hugo.poggetti
-- File description:
-- Main
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isNothing" #-}

module Utils (splitOne, splitOn, joinWithComma) where

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn delimiter s = case break (== delimiter) s of
    (h, []) -> [h]
    (h, t)  -> h : splitOn delimiter (drop 1 t)

splitOne :: Eq a => a -> [a] -> [[a]]
splitOne _ [] = []
splitOne delimiter s = case break (== delimiter) s of
    (h, []) -> [h]
    (h, t)  -> h : [drop 1 t]

joinWithComma :: [String] -> String
joinWithComma [] = ""
joinWithComma [x] = x
joinWithComma (x:xs) = x ++ "," ++ joinWithComma xs