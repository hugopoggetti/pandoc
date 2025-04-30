{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-hugo.poggetti
-- File description:
-- Main
-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Parsers.JsonParser (parseJson) where

import Lib
import Control.Applicative (Alternative(..))
import Ast.Document

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonString String
  | JsonNumber Int
  | JsonObject [(String, JsonValue)]
  | JsonArray [JsonValue]
  deriving (Show, Eq)

parseWhitespace :: Parser String
parseWhitespace = parseMany (parseAnyChar " \t\n\r")

parseJsonValue :: Parser JsonValue
parseJsonValue = parseWhitespace *> 
                (parseJsonNull <|> 
                 parseJsonBool <|> 
                 parseJsonNumber <|> 
                 parseJsonString <|> 
                 parseJsonArray <|> 
                 parseJsonObject) 
                <* parseWhitespace

-- | Parse JSON null
parseJsonNull :: Parser JsonValue
parseJsonNull = JsonNull <$ parseString "null"

-- | Parse JSON boolean
parseJsonBool :: Parser JsonValue
parseJsonBool = (JsonBool True <$ parseString "true") <|> 
                (JsonBool False <$ parseString "false")

-- | Parse JSON number
parseJsonNumber :: Parser JsonValue
parseJsonNumber = JsonNumber <$> parseInt

parseEscapeChar :: Parser Char
parseEscapeChar = parseChar '\\' *> parseAnyChar "\"\\/bfnrt" >>= \c ->
    return $ case c of
        'b' -> '\b'
        'f' -> '\f'
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
        c'  -> c'

parseStringChar :: Parser Char
parseStringChar = parseEscapeChar <|> 
    parseAnyChar (filter (/= '"') ['\32'..'\126'])

parseJsonString :: Parser JsonValue
parseJsonString = JsonString <$> (parseChar '"' *>
    parseMany parseStringChar <* parseChar '"')

parseJsonArray :: Parser JsonValue
parseJsonArray = JsonArray <$> (
    parseChar '[' *> parseWhitespace *> (parseJsonElements <|> pure []) 
        <* parseWhitespace <*parseChar ']')
  where
    parseJsonElements = parseAndWith (:) parseJsonValue (
        parseMany (parseWhitespace *> parseChar ','
            *> parseWhitespace *> parseJsonValue) >>= \rest ->
        return (rest))

parseJsonObject :: Parser JsonValue
parseJsonObject = JsonObject <$> (
    parseChar '{' *> parseWhitespace *> (parseJsonPairs <|> pure [])
        <* parseWhitespace <*parseChar '}')
  where
    parseJsonPairs = parseAndWith (:) parseJsonPair (
        parseMany (parseWhitespace *> parseChar ','
            *> parseWhitespace *> parseJsonPair) >>= \rest ->
        return (rest))
    
    parseJsonPair = do
      key <- parseChar '"' *> parseMany parseStringChar <* parseChar '"'
      _ <- parseWhitespace
      _ <- parseChar ':'
      _ <- parseWhitespace
      value <- parseJsonValue
      return (key, value)

runJsonParser :: String -> Maybe JsonValue
runJsonParser input = case runParser parseJsonValue input of
    Just (value, "") -> Just value
    Just (value, rest) -> if all (`elem` " \t\n\r") rest
                          then Just value
                          else Nothing
    Nothing -> Nothing

jsonToDocument :: JsonValue -> Document
jsonToDocument jsonValue = case jsonValue of
    JsonObject obj -> processObject obj
    _ -> Document defaultMeta []

processObject :: [(String, JsonValue)] -> Document
processObject obj = 
    let headerObj = findObject "header" obj
        bodyArray = findArray "body" obj
        meta = processHeader headerObj
        blocks = case bodyArray of
            Just array -> processBody array
            Nothing -> []
    in Document meta blocks

processHeader :: Maybe [(String, JsonValue)] -> Meta
processHeader Nothing = defaultMeta
processHeader (Just headerObj) =
    Meta { 
        metaTitle = processInlines (findString "title" headerObj),
        metaAuthors = maybe []
            (\s -> [processInlines (Just s)]) (findString "author" headerObj),
        metaDate = processInlines (findString "date" headerObj)
    }

processBody :: [JsonValue] -> [Block]
processBody bodyItems = concatMap processBodyItem bodyItems

processBodyItem :: JsonValue -> [Block]
processBodyItem (JsonArray inlineArray) = 
    [Para (concatMap processArrayItem inlineArray)]
processBodyItem (JsonObject obj) = 
    case findObject "section" obj of
        Just sectionObj -> [processSection sectionObj]
        Nothing -> 
            case findArray "list" obj of
                Just items -> [BulletList (map processListItem items)]
                Nothing ->
                    case findString "codeblock" obj of
                        Just code -> [CodeBlock code]
                        Nothing -> [Null]
processBodyItem _ = [Null]

processSection :: [(String, JsonValue)] -> Block
processSection obj = 
    let title = findString "title" obj
        contentArray = findArray "content" obj
        content = case contentArray of
            Just arr -> processBody arr
            Nothing -> []
    in Section 1 (processInlines title) content

processListItem :: JsonValue -> [Block]
processListItem (JsonArray inlines) =
    [Para (concatMap processArrayItem inlines)]
processListItem _ = [Null]

processArrayItem :: JsonValue -> [Inline]
processArrayItem (JsonString str) = [Str str]
processArrayItem (JsonObject obj) =
    processBold obj
    <|> processItalic obj
    <|> processCode obj
    <|> getLink obj
    <|> getImage obj
    <|> []
processArrayItem _ = []

processBold :: [(String, JsonValue)] -> [Inline]
processBold obj = maybeToList $ do
    text <- findString "bold" obj
    return (Strong [Str text])

processItalic :: [(String, JsonValue)] -> [Inline]
processItalic obj = maybeToList $ do
    text <- findString "italic" obj
    return (Emph [Str text])

processCode :: [(String, JsonValue)] -> [Inline]
processCode obj = maybeToList $ do
    text <- findString "code" obj
    return (Code text)

getLink :: [(String, JsonValue)] -> [Inline]
getLink obj = case findObject "link" obj of
    Just linkObj -> 
        let url = findString "url" linkObj
            contentArray = findArray "content" linkObj
            content = maybe [] (concatMap processArrayItem) contentArray
        in [Link content (maybe "" id url, "")]
    Nothing -> []

getImage :: [(String, JsonValue)] -> [Inline]
getImage obj = case findObject "image" obj of
    Just imgObj ->
        let url = findString "url" imgObj
            alt = maybe [] (concatMap processArrayItem) 
                (findArray "alt" imgObj)
        in [Image alt (maybe "" id url, "")]
    Nothing -> []

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing  = []

processLink :: [(String, JsonValue)] -> [Inline]
processLink obj = 
    let url = findString "url" obj
        contentArray = findArray "content" obj
        content = maybe [] (concatMap processArrayItem) contentArray
    in [Link content (maybe "" id url, "")]

processImage :: [(String, JsonValue)] -> [Inline]
processImage obj = 
    let url = findString "url" obj
        altArray = findArray "alt" obj
        alt = maybe [] (concatMap processArrayItem) altArray
    in [Image alt (maybe "" id url, "")]

processInlines :: Maybe String -> [Inline]
processInlines Nothing = []
processInlines (Just str) = [Str str]

findString :: String -> [(String, JsonValue)] -> Maybe String
findString key obj = case lookup key obj of
    Just (JsonString str) -> Just str
    _ -> Nothing

findObject :: String -> [(String, JsonValue)] -> Maybe [(String, JsonValue)]
findObject key obj = case lookup key obj of
    Just (JsonObject o) -> Just o
    _ -> Nothing

findArray :: String -> [(String, JsonValue)] -> Maybe [JsonValue]
findArray key obj = case lookup key obj of
    Just (JsonArray arr) -> Just arr
    _ -> Nothing

defaultMeta :: Meta
defaultMeta = Meta { metaTitle = [], metaAuthors = [], metaDate = [] }

parseJsonToDocument :: String -> Maybe Document
parseJsonToDocument input = do
    jsonValue <- runJsonParser input
    return (jsonToDocument jsonValue)

parseJson :: String -> Document -> Maybe Document
parseJson input _ = parseJsonToDocument input
