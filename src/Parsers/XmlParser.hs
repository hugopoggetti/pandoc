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
import Text.Read (readMaybe)
import Data.Maybe (fromJust)
import Debug.Trace

-- | Basic XML structure
data XmlValue
  = XmlElement String [(String, String)] [XmlValue]
  | XmlText String
  deriving (Show, Eq)

-- | Basic XML parsing
parseXmlValue :: Parser XmlValue
parseXmlValue =
   parseWhitespace *> (parseXmlElement <|> parseXmlText) <* parseWhitespace

parseXmlText :: Parser XmlValue
parseXmlText = XmlText <$> parseSome (parseAnyCharExcept "<")

parseXmlElement :: Parser XmlValue
parseXmlElement = do
  _ <- parseChar '<'
  name <- parseSome (parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))
  attribute <- parseMany parseXmlAttribute
  _ <- parseChar '>'
  blocks <- parseMany parseXmlValue
  _ <- parseString "</"
  _ <- parseString name
  _ <- parseChar '>'
  return $ XmlElement name attribute blocks

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
xmlToDocument (XmlElement "document" _ block) =
  let meta = getmeta block
      blocks = getblocks block
  in Document meta blocks
xmlToDocument _ = Document defaultMeta []

getmeta :: [XmlValue] -> Meta
getmeta block = case findElement "header" block of
  Just (XmlElement _ attrs headerblocks) ->
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
  Just (XmlElement _ _ body) -> concatMap getblock body
  _ -> []

getblock :: XmlValue -> [Block]
getblock (XmlElement "paragraph" _ inlines) =
   [Para (concatMap getInline inlines)]
getblock (XmlElement "section" attribute block) =
  let title = maybe [] (\t -> [Str t]) (lookup "title" attribute)
      content = concatMap getblock block
      sectionLevel = extractSectionLevel (fromJust(lookup "title" attribute))
  in [Section sectionLevel title content]
getblock (XmlElement "list" _ items) = [BulletList
   (map (\(XmlElement _ _ is) -> [Para (concatMap getInline is)]) items)]
getblock (XmlElement "codeblock" _ [XmlElement _ _ [XmlText current]]) =
  [CodeBlock current]
getblock _ = [Null]

extractSectionLevel :: String -> Int
extractSectionLevel title =
  case words title of
    (_ : levelStr : _) ->
      case readMaybe levelStr of
        Just level -> level
        Nothing -> 0
    _ -> 0

getInline :: XmlValue -> [Inline]
getInline (XmlText text) = [Str text]
getInline (XmlElement "bold" _ inline) = [Strong (concatMap getInline inline)]
getInline (XmlElement "italic" _ inline) = [Emph (concatMap getInline inline)]
getInline (XmlElement "code" _ [XmlText txt]) = [Code txt]
getInline (XmlElement "link" attrs inline) =
  let url = maybe "" id (lookup "url" attrs)
      content = concatMap getInline inline
  in [Link content (url, "")]
getInline (XmlElement "image" attrs inline) =
  let url = maybe "" id (lookup "url" attrs)
      alt = concatMap getInline inline
  in [Image alt (url, "")]
getInline _ = []

findElement :: String -> [XmlValue] -> Maybe XmlValue
findElement _ [] = Nothing
findElement name (x@(XmlElement n _ _) : xs)
  | name == n = Just x
  | otherwise = findElement name xs
findElement name (_:xs) = findElement name xs

extractText :: String -> [XmlValue] -> Maybe String
extractText name inline = case findElement name inline of
  Just (XmlElement _ _ [XmlText txt]) -> Just txt
  _ -> Nothing

defaultMeta :: Meta
defaultMeta = Meta { metaTitle = [], metaAuthors = [], metaDate = [] }

runXmlParser :: String -> Maybe XmlValue
runXmlParser input = case runParser parseXmlValue input of
  Just (val, "") -> Just val
  Just (val, rest) -> if all (`elem` " \t\n\r") rest then Just val else Nothing
  Nothing -> Nothing

parseXmlToDocument :: String -> Maybe Document
parseXmlToDocument input = do
  xmlVal <- runXmlParser input
  return (xmlToDocument xmlVal)

parsexml :: String -> Document -> Maybe Document
parsexml input _ = parseXmlToDocument input
