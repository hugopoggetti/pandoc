{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-hugo.poggetti
-- File description:
-- Main
-}

module Parsers.MarkdownParser (parsemd) where

import Lib
import Control.Applicative (Alternative(..))
import Ast.Document
import Data.List (isPrefixOf)
import Utils (splitOne)
import Data.Maybe (fromMaybe)

parsemd :: String -> Document -> Maybe Document
parsemd input _ = parseMarkdownToDocument input

parseMarkdownToDocument :: String -> Maybe Document
parseMarkdownToDocument input =
    let (meta, body) = parseMetadataAndBody input
        blocks = parseMarkdownBlocks body
    in Just (Document meta blocks)

parseMetadataAndBody :: String -> (Meta, String)
parseMetadataAndBody input =
    case runParser (parseString "---\n") input of
        Just (_, rest) -> parseMetadataWithBody rest
        Nothing -> (defaultMeta, input)

parseMetadataWithBody :: String -> (Meta, String)
parseMetadataWithBody rest =
    let (meta, restBody) = parseMetadata rest
    in case runParser (parseString "---\n") restBody of
        Just (_, body) -> (meta, body)
        Nothing -> (meta, restBody)

parseMetadata :: String -> (Meta, String)
parseMetadata input =
    let lines = extractMetadataLines input
        meta = buildMeta input
        trace 
        --rest = extractRestBody input lines
    in (meta, "")

extractMetadataLines :: String -> [String]
extractMetadataLines input = takeWhile (/= "---") (splitOne '\n' input)

extractRestBody :: String -> [String] -> String
extractRestBody input lines = unlines (drop (length lines + 1) (splitOne '\n' input))

buildMeta :: String -> Meta
buildMeta file =
    let (title, rest2) = parseTitle file
        (author, rest3) = parseAuthor rest2
        (date, _) = parseDate rest3
    in Meta { metaTitle = [Str title], metaAuthors = [[Str author]], metaDate = [Str date] }

parseTitle :: String -> (String , String)
parseTitle rest = 
    case runParser (parseString "title:") rest of
        Just (res, rest) ->
            let suce = splitOne '\n' rest
            in (head suce, last suce)
        Nothing -> error "Failed to parse title"

parseAuthor :: String -> (String , String)
parseAuthor rest =
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

parseMetadataLines :: [String] -> [(String, String)]
parseMetadataLines = map parseMetadataLine

parseMetadataLine :: String -> (String, String)
parseMetadataLine line =
    let (key, value) = break (== ':') line
    in (key, drop 2 value)

parseMarkdownBlocks :: String -> [Block]
parseMarkdownBlocks content = parseMarkdownLines (splitOne '\n' content)

parseMarkdownLines :: [String] -> [Block]
parseMarkdownLines [] = []
parseMarkdownLines (line:rest)
    | if null line then True else take 1 line == "#" = parseHeader (length (takeWhile (== '#') line)) line rest
    | take 3 line == "```" = parseCodeBlock rest
    | take 2 line == "- " = parseBulletList (line:rest)
    | "[" `isPrefixOf` line = parseLink line rest
    | "!" `isPrefixOf` line && "[[" `isPrefixOf` drop 1 line = parseImage line rest
    | otherwise = parseParagraph line rest

parseHeader :: Int -> String -> [String] -> [Block]
parseHeader level line rest =
    Header level [Str (drop (level + 1) line)] : parseMarkdownLines rest

parseCodeBlock :: [String] -> [Block]
parseCodeBlock rest =
    let (codeBlock, remaining) = span (/= "```") rest
    in CodeBlock (unlines codeBlock) : parseMarkdownLines (drop 1 remaining)

parseBulletList :: [String] -> [Block]
parseBulletList lines =
    let (listItems, remaining) = span (\l -> take 2 l == "- ") lines
    in BulletList (map (\item -> [Plain [Str (drop 2 item)]]) listItems) : parseMarkdownLines remaining

parseLink :: String -> [String] -> [Block]
parseLink line rest =
    let (text, url) = parseMarkdownLink line
    in Para [Link [Str text] (url, "")] : parseMarkdownLines rest

parseImage :: String -> [String] -> [Block]
parseImage line rest =
    let (alt, src) = parseMarkdownImage line
    in Para [Image [Str alt] (src, "")] : parseMarkdownLines rest

parseParagraph :: String -> [String] -> [Block]
parseParagraph line rest =
    Para [parseMarkdownInline line] : parseMarkdownLines rest

parseMarkdownInline :: String -> Inline
parseMarkdownInline line
    | take 2 line == "**" && drop (length line - 2) line == "**" = parseStrong line
    | take 1 line == "*" && drop (length line - 1) line == "*" = parseEmph line
    | take 1 line == "`" && drop (length line - 1) line == "`" = parseCode line
    | otherwise = Str line

parseStrong :: String -> Inline
parseStrong line =
    let content = drop 2 (take (length line - 2) line)
    in Strong [Str content]

parseEmph :: String -> Inline
parseEmph line =
    let content = drop 1 (take (length line - 1) line)
    in Emph [Str content]

parseCode :: String -> Inline
parseCode line =
    let content = drop 1 (take (length line - 1) line)
    in Code content

parseMarkdownLink :: String -> (String, String)
parseMarkdownLink line =
    let textStart = drop 1 (dropWhile (/= '[') line)
        text = takeWhile (/= ']') textStart
        urlStart = drop 2 (dropWhile (/= '(') line)
        url = takeWhile (/= ')') urlStart
    in (text, url)

parseMarkdownImage :: String -> (String, String)
parseMarkdownImage line =
    let altStart = drop 2 (dropWhile (/= '[') line)
        alt = takeWhile (/= ']') altStart
        srcStart = drop 2 (dropWhile (/= '(') line)
        src = takeWhile (/= ')') srcStart
    in (alt, src)

defaultMeta :: Meta
defaultMeta = Meta { metaTitle = [], metaAuthors = [], metaDate = [] }
