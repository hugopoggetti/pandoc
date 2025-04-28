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
import Data.Maybe (fromJust)

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

parseauthor :: String -> (String, Maybe String)
parseauthor file =
   case runParser (parseString "<date>") file of
    Just (_, rest) ->
      let dateParts = splitOn '<' rest
          date=if not (null dateParts)then head dateParts else "Nothing"
      in case runParser (parseString "</date>\n") ("<"++last dateParts) of
         Just (_, rest) ->if date == "Nothing" then (rest, Nothing)
            else (rest, Just date)
         Nothing -> (rest, Nothing)
    Nothing -> (file, Nothing)

parsedate :: String -> (String, Maybe String)
parsedate file =
   case runParser (parseString "<author>") file of
    Just (_, rest) ->
      let authParts = splitOn '<' rest
          auth=if not (null authParts)then head authParts else "Nothing"
      in case runParser (parseString "</author>\n") ("<"++last authParts) of
         Just (_, rest) -> if auth == "Nothing" then (rest, Nothing)
            else (rest, Just auth)
         Nothing -> (rest, Nothing)
    Nothing -> (file, Nothing)

getDateAndAuth :: String -> (String, Maybe String, Maybe String)
getDateAndAuth file =
   case runParser (parseString ">\n") file of
      Just (_, rest) ->
         let (afile, auth) = parseauthor rest
             (dfile, date) = parsedate afile
         in case runParser (parseString "</header>\n") file of
            Just (_, rest) -> (rest, auth, date)
            Nothing -> (file, Nothing, Nothing)
      Nothing -> (file, Nothing, Nothing)


parseheader :: String -> (String, Maybe String, Maybe String, Maybe String)
parseheader file =
  case runParser (parseString "<header title=\"") file of
    Just (_, rest) ->
      let titleParts = splitOn '\"' rest
          title=if not(null titleParts)then head titleParts++"\""else"Nothing"
      in  
         let (headerless, auth, date) = getDateAndAuth (last titleParts)
         in if title == "Nothing" then (headerless, Nothing, auth, date)
            else (headerless, Just title, auth, date)
    Nothing -> ("", Nothing, Nothing, Nothing)

getheader :: (String, Document) -> (String, Document)
getheader (file, newfile) = 
   let (headerless, title, auth, date) = parseheader file
       newMeta = Meta {
         metaTitle = [Str (fromJust title)],
         metaAuthors = [[Str (fromJust auth)]],
         metaDate = [Str (fromJust date)]
      }
   in (headerless ,Document newMeta (case newfile of Document _ blocks -> blocks))


getroot :: (String, Document) -> (String, Document)
getroot (file, newfile) =
   case runParser (parseString "<document/>\n") file of
      Just (_, rest) -> (rest, newfile)
      Nothing -> (file, newfile)

parsingxml :: (String, Document) -> Document
parsingxml (file, newfile) = head
  where
    (nfile, ndoc) = getroot (file, newfile)
    (headless, head) = getheader (nfile, ndoc)

parsexml :: String -> Document -> Document
parsexml file newfile = parsingxml (file, newfile)
    -- case runParser (parseChar '<') file of
    --     Just (result, rest) -> trace ("Parsed character: " ++ show result ++ ", remaining string: " ++ rest)
    --     Nothing -> trace "Failed to parse input"
    --trace ("the file: " ++ show )--(getXmlTagContent "author" file))
