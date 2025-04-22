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
jsonRenderInlinesQ = concatMap jsonRenderInline

jsonRenderInlines :: [Inline] -> String
jsonRenderInlines xs = "[" ++ intercalate ", " (map jsonRenderInline xs) ++ "]"

-- take a string and return a json string
toJson :: String -> String
toJson s = "\"" ++ s ++ "\""

showMathType :: MathType -> String
showMathType DisplayMath = "\"DisplayMath\""
showMathType InlineMath = "\"InlineMath\""

-- Helper function to render QuoteType as a string
showQuoteType :: QuoteType -> String
showQuoteType SingleQuote = "\"SingleQuote\""
showQuoteType DoubleQuote = "\"DoubleQuote\""

-- Helper function to render Attr as JSON
jsonRenderAttr :: Attr -> String
jsonRenderAttr (aid, classes, attributes) = 
  "{\"id\": " ++ toJson aid ++
  ", \"classes\": [" ++ concatMap toJson classes ++ "]" ++
  ", \"attributes\": [" ++ concatMap renderAttribute attributes ++ "]" ++
  "}"
  where
    renderAttribute (key, val) = "{\"" ++ key ++ "\": " ++ toJson val ++ "}"

-- | Render citation to JSON format
jsonRenderCitation :: Citation -> String
jsonRenderCitation (Citation cid prefix suffix mode noteNum hash) =
  "{"
    ++ "\"id\": " ++ toJson cid ++ ", "
    ++ "\"prefix\": " ++ jsonRenderInlines prefix ++ ", "
    ++ "\"suffix\": " ++ jsonRenderInlines suffix ++ ", "
    ++ "\"mode\": " ++ renderCitationMode mode ++ ", "
    ++ "\"noteNum\": " ++ show noteNum ++ ", "
    ++ "\"hash\": " ++ show hash
  ++ "}"

-- | get citation mode 
renderCitationMode :: CitationMode -> String
renderCitationMode AuthorInText = toJson "AuthorInText"
renderCitationMode SuppressAuthor = toJson "SuppressAuthor"
renderCitationMode NormalCitation = toJson "NormalCitation"

-- | Recursive func to generate inline in json format with all conventions 
jsonRenderInline :: Inline -> String
jsonRenderInline (Str s) = toJson (escapeQuotes s) 
jsonRenderInline (Emph xs) = "{\"Emph\": " ++ jsonRenderInlines xs ++ "}"
jsonRenderInline (Strong xs) = "{\"Strong\": " ++ jsonRenderInlines xs ++ "}"
jsonRenderInline (Strikeout xs) = "{\"Strikeout\": "
    ++ jsonRenderInlines xs ++ "}"
jsonRenderInline (Superscript xs) = "{\"Superscript\": "
    ++ jsonRenderInlines xs ++ "}"
jsonRenderInline (Subscript xs) = "{\"Subscript\": " 
    ++ jsonRenderInlines xs ++ "}"
jsonRenderInline (SmallCaps xs) = "{\"SmallCaps\": "
    ++ jsonRenderInlines xs ++ "}"
jsonRenderInline (Quoted qt xs) = "{\"Quoted\": {\"type\": " ++ 
    showQuoteType qt ++ ", \"content\": " ++ jsonRenderInlines xs ++ "}}"
jsonRenderInline (Cite citations xs) = 
  "{\"Cite\": {"
    ++ "\"citations\": [" ++ intercalate ", " 
        (map jsonRenderCitation citations) ++ "], "
    ++ "\"content\": " ++ jsonRenderInlines xs
  ++ "}}"
jsonRenderInline (Code attr s) = 
  "{\"Code\": {\"attr\": " ++ jsonRenderAttr attr ++ ", \"content\": "
    ++ toJson (escapeQuotes s) ++ "}}"
jsonRenderInline Space = "{\"Space\": {}}"
jsonRenderInline SoftBreak = "{\"SoftBreak\": {}}"
jsonRenderInline LineBreak = "{\"LineBreak\": {}}"
jsonRenderInline (Math mt s) = "{\"Math\": {\"type\": "
    ++ showMathType mt ++ ", \"content\": " ++ toJson s ++ "}}"
jsonRenderInline (RawInline (Format f) s) =
  "{\"RawInline\": {"
    ++ "\"format\": " ++ toJson f ++ ", "
    ++ "\"content\": " ++ toJson (escapeQuotes s)
  ++ "}}"
jsonRenderInline (Link attr xs (url, title)) = 
  "{\"Link\": {"
    ++ "\"attr\": " ++ jsonRenderAttr attr ++ ", "
    ++ "\"text\": " ++ jsonRenderInlines xs ++ ", "
    ++ "\"target\": {\"url\": " ++ toJson url ++ ", \"title\": "
    ++ toJson title ++ "}" ++ "}}"
jsonRenderInline (Image attr xs (url, title)) = 
  "{\"Image\": {"
    ++ "\"attr\": " ++ jsonRenderAttr attr ++ ", "
    ++ "\"altText\": " ++ jsonRenderInlines xs ++ ", "
    ++ "\"target\": {\"url\": " ++ toJson url ++ ", \"title\": "
    ++ toJson title ++ "}" ++ "}}"
jsonRenderInline (Span attr xs) = 
  "{\"Span\": {"
    ++ "\"attr\": " ++ jsonRenderAttr attr ++ ", "
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

renderListAttributes :: ListAttributes -> String
renderListAttributes (start, style, delim) =
  "{"
    ++ "\"start\": " ++ show start ++ ", "
    ++ "\"style\": " ++ toJson (show style) ++ ", "
    ++ "\"delimiter\": " ++ toJson (show delim)
  ++ "}"

renderDefinition :: ([Inline], [[Block]]) -> String
renderDefinition (term, defs) =
  "{"
    ++ "\"term\": " ++ jsonRenderInlines term ++ ", "
    ++ "\"definitions\": [" ++ intercalate ", " 
        (map jsonRenderBody defs) ++ "]" ++ "}"

-- | render block in json format
jsonRenderBlock :: Block -> String
jsonRenderBlock (Plain xs) = "{\"Plain\": " ++ jsonRenderInlines xs ++ "}"
jsonRenderBlock (Para xs) = "{\"Para\": " ++ jsonRenderInlines xs ++ "}"
jsonRenderBlock (CodeBlock attr str) = 
  "{\"CodeBlock\": {\"attr\": " ++ jsonRenderAttr attr ++ ", \"content\": "
    ++ toJson (escapeQuotes str) ++ "}}"
jsonRenderBlock (RawBlock (Format f) str) = 
  "{\"RawBlock\": {\"format\": " ++ toJson f ++ ", \"content\": "
    ++ toJson (escapeBackslashes str) ++ "}}"
jsonRenderBlock (BlockQuote blocks) = 
  "{\"BlockQuote\": [" ++ intercalate ", " (map jsonRenderBlock blocks) ++ "]}"
jsonRenderBlock (OrderedList attrs items) = 
  "{\"OrderedList\": {\"attributes\": " ++ renderListAttributes attrs ++
    ", \"items\": [" ++ intercalate ", " (map jsonRenderBody items) ++ "]}}"
jsonRenderBlock (BulletList items) = 
  "{\"BulletList\": [" ++ intercalate ", " (map jsonRenderBody items) ++ "]}"
jsonRenderBlock (DefinitionList defs) = 
  "{\"DefinitionList\": [" ++ intercalate ", "
    (map renderDefinition defs) ++ "]}"
jsonRenderBlock (Header level attr xs) = 
  "{\"Header\": {\"level\": " ++ show level ++ ", \"attr\": " ++ jsonRenderAttr
    attr ++ ", \"content\": " ++ jsonRenderInlines xs ++ "}}"
jsonRenderBlock HorizontalRule = "{\"HorizontalRule\": {}}"
jsonRenderBlock (Div attr blocks) = 
  "{\"Div\": {\"attr\": " ++ jsonRenderAttr attr ++ ", \"content\": [" ++ 
    intercalate ", " (map jsonRenderBlock blocks) ++ "]}}"
jsonRenderBlock Null = "null"

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
            "\"Authors\": " ++ inlineArToJsonArray auth 
        dateStr   = if null date then "" else
            "\"Date\": " ++ jsonRenderInlinesQ date
        parts = filter (not . null) [titleStr, authStr, dateStr]
    in toJson "header" ++ ": {" ++ intercalate ",\n" parts ++ "\n},\n"

-- | render body with list of blocks
jsonRenderBody :: [Block] -> String
jsonRenderBody blocks = "[" ++ intercalate ", "
    (map jsonRenderBlock blocks) ++ "]"

-- | Render entire Document into jsondown format
jsonRender :: Document -> String
jsonRender (Document meta blocks) = 
   "{\n" ++ jsonRenderTitle meta ++ toJson "body" ++ ": " 
     ++ jsonRenderBody blocks ++ "\n" ++ "}\n"
