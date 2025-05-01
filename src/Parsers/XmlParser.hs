{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-hugo.poggetti
-- File description:
-- XML Parser
-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Parsers.XmlParser (parsexml) where

import Lib
import Control.Applicative (Alternative(..))
import Ast.Document
import Data.Maybe (fromJust)

-- | Basic XML structure
data XmlValue
  = XmlElement String Int [(String, String)] [XmlValue]
  | XmlText String
  deriving (Show, Eq)

-- | Basic XML parsing
parseXmlValue :: Int -> Parser XmlValue
parseXmlValue level =
   parseXmlElement level<|> parseXmlText

parseXmlText :: Parser XmlValue
parseXmlText = XmlText <$> parseSome (parseAnyCharExcept "<")

parseXmlElement :: Int -> Parser XmlValue
parseXmlElement level = do
  _ <- parseChar '<'
  name <- parseSome (parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))
  attribute <- parseMany parseXmlAttribute
  _ <- parseChar '>'
  let nlevel = getlevel level name
  blocks <- parseMany (parseXmlValue nlevel)
  _ <- parseString "</"
  _ <- parseString name
  _ <- parseChar '>'
  return $ XmlElement name level attribute blocks

getlevel :: Int -> String -> Int
getlevel level "section" = level + 1
getlevel level _ = level


parseXmlAttribute :: Parser (String, String)
parseXmlAttribute = do
  _ <- parseWhitespace
  headerc <- parseSome (parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))
  _ <- parseChar '='
  _ <- parseChar '"'
  value <- parseMany (parseAnyCharExcept "\"")
  _ <- parseChar '"'
  return (headerc, value)

parseWhitespace :: Parser String
parseWhitespace = parseMany (parseAnyChar " \t\n\r")

xmlToDocument :: XmlValue -> Document
xmlToDocument (XmlElement "document"_  _ block) =
  let meta = getmeta block
      blocks = getblocks block
  in Document meta blocks
xmlToDocument _ = Document defaultMeta []

getmeta :: [XmlValue] -> Meta
getmeta block = case findElement "header" block of
  Just (XmlElement _ _ attrs headerblocks) ->
    let title = lookup "title" attrs
        author = extractText "author" headerblocks
        date = extractText "date" headerblocks
    in Meta
      { metaTitle = [Str (fromJust title) | not (title == Nothing)]
      , metaAuthors = [[Str (fromJust author) | not (title == Nothing)]]
      , metaDate = [Str (fromJust date) | not (title == Nothing)] }
  _ -> defaultMeta

getblocks :: [XmlValue] -> [Block]
getblocks block = case findElement "body" block of
  Just (XmlElement _ _ _ body) -> concatMap getblock body
  _ -> []

getblock :: XmlValue -> [Block]
getblock (XmlElement "paragraph"_ _ inlines) =
   [Para (concatMap getInline inlines)]

getblock (XmlElement "section" level attribute block) =
  let title = maybe [] (\t -> [Str t]) (lookup "title" attribute)
      content = concatMap getblock block
  in [Section level title content]

getblock (XmlElement "list"_ _ items) = [BulletList
   (map (\(XmlElement _ _ _ is) -> [Para (concatMap getInline is)]) items)]
getblock (XmlElement "codeblock"_ _ [XmlElement _ _ _ [XmlText current]]) =
  [CodeBlock current]
getblock _ = [Null]

getInline :: XmlValue -> [Inline]
getInline (XmlText text) = [Str text]
getInline (XmlElement "bold"_ _ inline) = [Strong (concatMap getInline inline)]
getInline (XmlElement "italic"_ _ inline) = [Emph (concatMap getInline inline)]
getInline (XmlElement "code"_ _ [XmlText txt]) = [Code txt]
getInline (XmlElement "link"_ attrs inline) =
  let url = maybe "" id (lookup "url" attrs)
      content = concatMap getInline inline
  in [Link content (url, "")]
getInline (XmlElement "image"_ attrs inline) =
  let url = maybe "" id (lookup "url" attrs)
      alt = concatMap getInline inline
  in [Image alt (url, "")]
getInline _ = []

findElement :: String -> [XmlValue] -> Maybe XmlValue
findElement _ [] = Nothing
findElement name (x@(XmlElement n _ _ _) : xs)
  | name == n = Just x
  | otherwise = findElement name xs
findElement name (_:xs) = findElement name xs

extractText :: String -> [XmlValue] -> Maybe String
extractText name inline = case findElement name inline of
  Just (XmlElement _ _ _ [XmlText txt]) -> Just txt
  _ -> Nothing

defaultMeta :: Meta
defaultMeta = Meta { metaTitle = [], metaAuthors = [], metaDate = [] }

runXmlParser :: String -> Maybe XmlValue
runXmlParser input = case runParser (parseXmlValue 1) input of
  Just (val, "") -> Just val
  Just (val, rest) -> if all (`elem` " \t\n\r") rest then Just val else Nothing
  Nothing -> Nothing

parseXmlToDocument :: String -> Maybe Document
parseXmlToDocument input = do
  xmlVal <- runXmlParser input
  return (xmlToDocument xmlVal)

parsexml :: String -> Document -> Maybe Document
parsexml input _ = parseXmlToDocument input
