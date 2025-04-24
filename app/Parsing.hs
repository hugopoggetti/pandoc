{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-hugo.poggetti
-- File description:
-- Main
-}

module Parsing (parsefile) where

import OptsParserSystem
import Ast.Document
import Data.Maybe

Data Jsoncutter = Jsoncutter {
    obracket :: String,  -- Opening bracket for JSON object
    cbracket :: String,  -- Closing bracket for JSON object
    oarray   :: String,
    carray   :: String,
    colon    :: String,  -- Key-value separator ":"
    comma    :: String   -- Element separator ","
}

jsonCutterDefault :: Jsoncutter
jsonCutterDefault = Jsoncutter {
    obracket = "{",
    cbracket = "}",
    oarray = "[",
    carray = "]",
    colon = ":",
    comma = ","
}

Data Xmlcutter = Xmlcutter {
    tagWrap     :: String -> String,                    -- content in a single tag: <tag>content</tag>
    selfClosing :: String -> String,                    -- For tags like <br/>
    attribute   :: (String, String) -> String           -- Format attribute: key="value"
}

xmlCutterDefault :: Xmlcutter
xmlCutterDefault = Xmlcutter {
    tagWrap = \tag content -> "<" ++ tag ++ ">" ++ content ++ "</" ++ tag ++ ">",
    selfClosing = \tag -> "<" ++ tag ++ " />",
    attribute = \(k, v) -> k ++ "=\"" ++ v ++ "\""
}

Data Mdcutter = Mdcutter {
    meta       :: String,
    mTitle     :: String,
    mAuthor    :: String,
    mDate      :: String,
    header     :: Int -> String,
    bold       :: String -> String,
    italic     :: String -> String,
    code       :: String -> String,
    codeblock  :: String -> String,
    list       :: String -> String,
    link       :: (String, String) -> String,
    image      :: (String, String) -> String
}

mdcutterDefault :: Mdcutter
mdcutterDefault = Mdcutter {
    meta = "---",
    mTitle = "title:",
    mAuthor = "author:",
    mDate = "date:",
    header = \n -> if n >= 1 && n <= 6 then replicate n '#' else error "Invalid header level",
    bold = \txt -> "**" ++ txt ++ "**",
    italic = \txt -> "*" ++ txt ++ "*",
    code = \txt -> "`" ++ txt ++ "`",
    codeblock = \txt -> "```" ++ "\n" ++ txt ++ "\n```",
    list = \item -> "- " ++ item,
    link = \(text, url) -> "[" ++ text ++ "](" ++ url ++ ")",
    image = \(alt, src) -> "![" ++ alt ++ "](" ++ src ++ ")"
}



parsefile :: String -> Document -> Opts -> IO ()
parsefile content (Document meta blocks) opts = do
    putStrLn (fromJust (inputFormat opts)) 

    -- | (inputFormat opts) == "json" = 
    -- | (inputFormat opts) == "markdown" = 
    -- | (inputFormat opts) == "xml" = 

--   -
--   {}
--   <@name> </@name>