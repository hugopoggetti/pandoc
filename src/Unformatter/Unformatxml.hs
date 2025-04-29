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
import Debug.Trace

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

parsedate :: String -> (String, Maybe String)
parsedate file =
   case runParser (parseString "<date>") file of
    Just (_, rest) ->
      let dateParts = splitOne '<' rest
          date = head dateParts
      in case runParser (parseString "</date>\n") ("<"++last dateParts) of
         Just (_, rest) -> (removeLeadingSpaces rest, Just date)
         Nothing -> (rest, Nothing)
    Nothing -> (file, Nothing)

parseauthor :: String -> (String, Maybe String)
parseauthor file =
   case runParser (parseString "<author>") file of
    Just (_, rest) ->
      let authParts = splitOne '<' rest
          auth = head authParts
      in case runParser (parseString "</author>\n") ("<"++last authParts) of
         Just (_, rest) -> (removeLeadingSpaces rest, Just auth)
         Nothing -> (rest, Nothing)
    Nothing -> (file, Nothing)

getDateAndAuth :: String -> (String, Maybe String, Maybe String)
getDateAndAuth file =
   case runParser (parseString ">\n") file of
      Just (_, rest) ->
         let (afile, auth) = parseauthor (removeLeadingSpaces rest)
             (dfile, date) = parsedate afile
         in case runParser (parseString "</header>\n")
         (removeLeadingSpaces dfile) of
            Just (_, rest) -> (rest, auth, date)
            Nothing -> (file, Nothing, Nothing)
      Nothing -> (file, Nothing, Nothing)


parseheader :: String -> (String, Maybe String, Maybe String, Maybe String)
parseheader file =
  case runParser (parseString "<header title=\"") file of
    Just (_, rest) ->
      let titleParts = splitOne '\"' rest
          title= head titleParts
      in  
         let (headerless, auth, date) = getDateAndAuth (last titleParts)
         in (headerless, Just title, auth, date)
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
   case runParser (parseString "<document>\n") file of
      Just (_, rest) -> (removeLeadingSpaces rest, newfile)
      Nothing -> (file, newfile)

parsingxml :: (String, Document) -> Document
parsingxml (file, newfile) = head
  where
    (nfile, ndoc) = getroot (file, newfile)
    (headless, head) = getheader (nfile, ndoc)

parsexml :: String -> Document -> Document
parsexml file newfile = parsingxml (file, newfile)
