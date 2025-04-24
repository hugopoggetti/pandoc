{-
-- EPITECH PROJECT, 2022
-- jsonRender.hs
-- File description:
-- tt
-}

module Renders.JsonRender.JsonRender (jsonRender) where
import Ast.Document
import Data.List (intercalate)

-- | Render of json Inline list
jsonRenderInlinesQ :: [Inline] -> String
jsonRenderInlinesQ [] = "\"\""
jsonRenderInlinesQ a = concatMap jsonRenderInline a

jsonRenderInlines :: [Inline] -> String
jsonRenderInlines xs = "[" ++ intercalate ", " (map jsonRenderInline xs) ++ "]"

-- take a string and return a json string
toJson :: String -> String
toJson s = "\"" ++ s ++ "\""

-- | Recursive func to generate inline in json format with all conventions 
jsonRenderInline :: Inline -> String
jsonRenderInline (Str s) = toJson (escapeQuotes s) 
jsonRenderInline (Emph xs) = "{\"italic\": " ++ jsonRenderInlinesQ xs ++ "}"
jsonRenderInline (Strong xs) = "{\"bold\": " ++ jsonRenderInlinesQ xs ++ "}"
jsonRenderInline (Code s) = 
  "{\"code\": " ++ toJson (escapeQuotes s) ++ "}"
jsonRenderInline (RawInline (Format f) s) =
  "{\"rawInline\": {"
    ++ "\"format\": " ++ toJson f ++ ", "
    ++ "\"content\": " ++ toJson (escapeQuotes s)
  ++ "}}"
jsonRenderInline (Link xs (url, _)) = 
  "{\"link\": {" ++ toJson "url" ++ ": " ++ toJson url ++ "," ++ 
    toJson "content" ++ ": " ++ jsonRenderInlines xs ++ "}}" 
jsonRenderInline (Image xs (url, _)) = 
    "{\"image\": {" ++ toJson "url" ++ ": " ++ toJson url ++ "," ++ 
    toJson "alt" ++ ": " ++ jsonRenderInlines xs ++ "}}" 
jsonRenderInline (Span xs) = 
  "{\"span\": {"
    ++ "\"content\": " ++ jsonRenderInlines xs
  ++ "}}"
jsonRenderInline _ = ""

-- Helper function to escape quotes inside a string
escapeQuotes :: String -> String
escapeQuotes = concatMap (\c -> if c == '\"' then "\\\"" else [c])

-- Helper function to escape backslashes and quotes in a string
escapeBackslashes :: String -> String
escapeBackslashes = concatMap escapeChar
  where
    escapeChar c = case c of
      '\\' -> "\\\\"
      '\"' -> "\\\""
      _    -> [c]

-- | Render bulletlist with list of block without []
jsonRenderList :: [Block] -> String
jsonRenderList blocks =  intercalate ", " (map jsonRenderBlock blocks)

-- | render block in json format
jsonRenderBlock :: Block -> String
jsonRenderBlock (Plain xs) = jsonRenderInlines xs
jsonRenderBlock (Para xs) =  jsonRenderInlines xs
jsonRenderBlock (CodeBlock str) = 
  "{\"codeblock\": [" ++ toJson (escapeQuotes str) ++ "]}"
jsonRenderBlock (RawBlock (Format f) str) = 
  "{\"rawblock\": {\"format\": " ++ toJson f ++ ", \"content\": "
    ++ toJson (escapeBackslashes str) ++ "}}"
jsonRenderBlock (BulletList items) = 
  "{\"list\": [" ++ intercalate ", " (map jsonRenderList items) ++ "]}"
jsonRenderBlock (Header _ xs) =  
    "{\"title\": " ++ jsonRenderInlines xs ++ "}"
jsonRenderBlock (Section _  title content) =
    "{\"section\": {\"title\": " ++ jsonRenderInlinesQ title
    ++ ", \"content\": " ++ jsonRenderBody content ++ "}}"
jsonRenderBlock Null = "null"
jsonRenderBlock _ = ""

-- | take inline array and generate json String arr
inlineArToJsonArray :: [[Inline]] -> String
inlineArToJsonArray inlines = 
    let inlineStr = map jsonRenderInlinesQ inlines
    in "[" ++ intercalate", " inlineStr ++ "]" 

-- | header generation with Meta 
jsonRenderTitle :: Meta -> String
jsonRenderTitle (Meta title auth date) = 
    let 
        titleStr  = if null title then "" else 
            "\"title\": " ++ jsonRenderInlinesQ title
        authStr   = if null auth then "" else 
            "\"author\": " ++ inlineArToJsonArray auth 
        dateStr   = if null date then "" else
            "\"date\": " ++ jsonRenderInlinesQ date
        parts = filter (not . null) [titleStr, authStr, dateStr]
    in toJson "header" ++ ": {" ++ intercalate ",\n" parts ++ "\n}"

-- | render body with list of blocks
jsonRenderBody :: [Block] -> String
jsonRenderBody blocks = "[" ++ intercalate ", "
    (map jsonRenderBlock blocks) ++ "]"

-- | Render entire Document into jsondown format
jsonRender :: Document -> String
jsonRender (Document meta []) = "{\n" ++ jsonRenderTitle meta ++ "}\n"
jsonRender (Document meta blocks) = 
   "{\n" ++ jsonRenderTitle meta ++ ",\n"++ toJson "body" 
    ++ ": " ++ jsonRenderBody blocks ++ "\n}\n"
