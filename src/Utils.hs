{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-hugo.poggetti
-- File description:
-- Main
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isNothing" #-}

module Utils (splitOne, splitOn, joinWithComma, removeLeadingSpaces, push, pop, getiteration, splitstr) where
import Ast.Document (Inline)
import Data.List
import Data.Maybe (fromJust)

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

removeLeadingSpaces :: String -> String
removeLeadingSpaces = dropWhile (== ' ')

push :: a -> [a] -> [a]
push x stack = x : stack

pop :: [a] -> (Maybe a, [a])
pop []     = (Nothing, [])
pop (x:xs) = (Just x, xs)

safeIndex :: [String] -> Int -> Maybe String
safeIndex [] _ = Nothing
safeIndex (x:_) 0 = Just x
safeIndex (_:xs) n = safeIndex xs (n-1)

getiteration :: [String] -> String -> Int -> Int -> Int
getiteration array name index val
    | safeIndex array index == Nothing = val
    | fromJust(safeIndex array index) == name =
        getiteration array name (index+1) val + 1
    | otherwise = getiteration array name (index+1) val

splitstr :: String -> String -> (String, Maybe String)
splitstr delim str = case breakOn delim str of
    (before, "")    -> (before, Nothing)
    (before, rest)  -> (before, Just(drop (length delim) rest))

breakOn :: String -> String -> (String, String)
breakOn _ "" = ("", "")
breakOn delim s@(x:xs)
    | delim `isPrefixOf` s = ("", s)
    | otherwise = let (before, after) = breakOn delim xs
                  in (x : before, after)
