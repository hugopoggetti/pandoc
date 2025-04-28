{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-hugo.poggetti
-- File description:
-- Main
-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Unformatter.Unformatxml (parsexml) where

import Ast.Document (Document(..), Meta (..), newdoc, Inline (..))
import Lib (Parser(runParser), parseString)
import Utils

data Xmlcutter = Xmlcutter {
   header :: [String],
   body :: [String]
}

xmlCutterDefault :: Xmlcutter
xmlCutterDefault = Xmlcutter {
   header = ["<header title=", "<author>", "<date>"],
   body =  ["<paragraph>", "<section title=", "<bold>", "<italic>",
   "<code>", "<codeblock>", "<list>", "<link url=", "<image url="]
}

getauthor :: (String, Document) -> (String, Document)
getauthor (file, newfile) =
   case runParser (parseString "<author>") file of
    Just (result, rest) ->
      let authParts = splitOn '<' rest
          auth=if not (null authParts)then head authParts else "no auth"
          newMeta = Meta {metaAuthors = [[Str auth]]}
      in("<"++last authParts, Document newMeta (case newfile of Document _ blocks -> blocks))
    Nothing -> (file, newfile)

--getdate ::

-- newMeta = Meta {
--             metaTitle = [Str title],
--             metaAuthors = [[Str author]],
--             metaDate = [Str date]
--         }
--     in Document newMeta (case doc of Document _ blocks -> blocks)


parseheader :: (String, Document) -> (String, Document)
parseheader (file, newfile) =
  case runParser (parseString "<header title=\"") file of
    Just (result, rest) ->
      let titleParts = splitOn '\"' rest
          title=if not (null titleParts)then head titleParts++"\"" else "Untitl"
          newMeta = Meta { metaTitle = [Str title]}
      in(last titleParts, Document newMeta (case newfile of Document _ blocks -> blocks))
    Nothing -> (file, newfile)

getroot :: (String, Document) -> (String, Document)
getroot (file, newfile) =
   case runParser (parseString "<document/>\n") file of
      Just (result, rest) -> (rest, newfile)
      Nothing -> (file, newfile)

parsingxml :: (String, Document) -> Document
parsingxml (file, newfile) = newfile
  where
    (nfile, nnewfile) = getroot (file, newfile)

parsexml :: String -> Document -> Document
parsexml file newfile = parsingxml (file, newfile)
    -- case runParser (parseChar '<') file of
    --     Just (result, rest) -> trace ("Parsed character: " ++ show result ++ ", remaining string: " ++ rest)
    --     Nothing -> trace "Failed to parse input"
    --trace ("the file: " ++ show )--(getXmlTagContent "author" file))
