{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-hugo.poggetti
-- File description:
-- Main
-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Unformatter.Unformatxml () where

data Xmlcutter = Xmlcutter {
    tagWrap       :: String -> String -> String,
   -- attribute     :: (String, String) -> String,
    tagWithAttrs  :: String -> [String] -> String -> String,
    --tagList       :: String -> [String] -> String,
    metaXml       :: [(String, String)] -> String,
    headerXml     :: Int -> String -> String,
    textXml       :: String -> String -> String, -- e.g., tag-based formatting like <b>, <i>
    linkXml       :: (String, String) -> String,
    imageXml      :: (String, String) -> String
}

xmlCutterDefault :: Xmlcutter
xmlCutterDefault = Xmlcutter {
    tagWrap = \tag content -> "<" ++ tag ++ ">" ++ content ++ "</" ++ tag ++ ">",
    --attribute = \(k, v) -> k ++ "=\"" ++ v ++ "\"",
    tagWithAttrs = \tag attrs content -> "<" ++ tag ++ " " ++ unwords attrs ++ ">" ++ content ++ "</" ++ tag ++ ">",
    --tagList = \tag contents -> concatMap (tagWrap tag) contents,
    metaXml = \pairs -> "<meta>" ++ concatMap (\(k,v) -> "<" ++ k ++ ">" ++ v ++ "</" ++ k ++ ">") pairs ++ "</meta>",
    headerXml = \n txt -> "<h" ++ show n ++ ">" ++ txt ++ "</h" ++ show n ++ ">",
    textXml = \tag txt -> "<" ++ tag ++ ">" ++ txt ++ "</" ++ tag ++ ">",
    linkXml = \(text, url) -> "<a href=\"" ++ url ++ "\">" ++ text ++ "</a>",
    imageXml = \(alt, src) -> "<img alt=\"" ++ alt ++ "\" src=\"" ++ src ++ "\" />"
}
