{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-hugo.poggetti
-- File description:
-- Main
-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Unformatter.Unformatxml (parsexml) where

import Ast.Document (Document(..), Meta (..), newdoc, Inline (..), Block)
import Lib (Parser(runParser), parseString)
import Utils
import Data.Maybe (fromJust)
import Debug.Trace

data Xmlcutter = Xmlcutter {
   body :: [String]
}

xmlCutterDefault :: Xmlcutter
xmlCutterDefault = Xmlcutter {
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
         Nothing -> error "invalid balise"
    Nothing -> error "missing date"

parseauthor :: String -> (String, Maybe String)
parseauthor file =
   case runParser (parseString "<author>") file of
    Just (_, rest) ->
      let authParts = splitOne '<' rest
          auth = head authParts
      in case runParser (parseString "</author>\n") ("<"++last authParts) of
         Just (_, rest) -> (removeLeadingSpaces rest, Just auth)
         Nothing -> error "invalid balise"
    Nothing -> error "missing author"

getDateAndAuth :: String -> (String, Maybe String, Maybe String)
getDateAndAuth file =
   case runParser (parseString ">\n") file of
      Just (_, rest) ->
         let (afile, auth) = parseauthor (removeLeadingSpaces rest)
             (dfile, date) = parsedate afile
         in case runParser (parseString "</header>\n")
         (removeLeadingSpaces dfile) of
            Just (_, rest) -> (rest, auth, date)
            Nothing -> error "missing balise"
      Nothing -> error "invalid header"

parseheader :: String -> (String, Maybe String, Maybe String, Maybe String)
parseheader file =
  case runParser (parseString "<header title=\"") file of
    Just (_, rest) ->
      let titleParts = splitOne '\"' rest
          title= head titleParts
      in  
         let (headerless, auth, date) = getDateAndAuth (last titleParts)
         in ((removeLeadingSpaces headerless), Just title, auth, date)
    Nothing -> error "invalid header"

getheader :: (String, Document) -> (String, Document)
getheader (file, newfile) = 
   let (headerless, title, auth, date) = parseheader file
       newMeta = Meta {
         metaTitle = [Str (fromJust title)],
         metaAuthors = [[Str (fromJust auth)]],
         metaDate = [Str (fromJust date)]
      }
   in (headerless ,Document newMeta (case newfile of Document _ blocks -> blocks))

getroot :: (String, Document) -> (String, [String], Document)
getroot (file, newfile) =
   case runParser (parseString "<document>\n") file of
      Just (result, rest) -> (removeLeadingSpaces rest, [result], newfile)
      Nothing -> error "unknown root"

-- a
-- a
-- a
-- a
-- a
-- a
-- a
-- a
-- a

getbold :: (String, [String], [Block]) -> (String, [String], [Block])
getbold (file , array, block) = (file , array, block)

getitalic :: (String, [String], [Block]) -> (String, [String], [Block])
getitalic (file , array, block) = (file , array, block)

getcode :: (String, [String], [Block]) -> (String, [String], [Block])
getcode (file , array, block) = (file , array, block)

getlink :: (String, [String], [Block]) -> (String, [String], [Block])
getlink (file , array, block) = (file , array, block)

getimage :: (String, [String], [Block]) -> (String, [String], [Block])
getimage (file , array, block) = (file , array, block)

getcodeblock :: (String, [String], [Block]) -> (String, [String], [Block])
getcodeblock (file , array, block) = (file , array, block)

getlist :: (String, [String], [Block]) -> (String, [String], [Block])
getlist (file , array, block) = (file , array, block)

getparagraph :: (String, [String], [Block]) -> (String, [String], [Block])
getparagraph (file , array, block) = (file , array, block)

getsection :: (String, [String], [Block]) -> (String, [String], [Block]) -- get the others section and an inline
getsection (file , array, block) = (file , array, block)
--    case runParser (parseString "<section title=\"") file of
--     Just (_, rest) ->
--       let sectionParts = splitOne '>' rest
--           section = head sectionParts
--       in (removeLeadingSpaces rest, push "</section>" array, newdoc)
--     Nothing -> (file , array, newdoc)

isinline :: (String, [String], [Inline]) -> (String, [String], [Inline])--(bold italic code link image)
isinline (file, array, inline) = 

-- a
-- a

isaclosing :: (String, [String], [Block]) -> (String, [String], [Block])
isaclosing (file, array, block) =
  case runParser (parseString "</") file of
    Just (_, rest1) ->
      case array of
        (toclose:restArray) ->
          case runParser (parseString toclose) rest1 of
            Just (_, rest2) -> (removeLeadingSpaces rest2, restArray, block)
            Nothing -> error "bad file format"
        [] -> error "nothing to close"
    Nothing -> (file, array, block)

isablock :: (String, [String], [Block]) -> (String, [String], [Block]) --(prargraph section codeblock list)
isablock (file, array, block) =
    let (pfile, parray, pblock) = getparagraph (file, array, block)
    in if pfile /= file then (pfile, parray, pblock) else
    let (sfile, sarray, sblock) = getsection (file, array, block)
    in if sfile /= file then (sfile, sarray, sblock) else
    let (cfile, carray, cblock) = getcodeblock (file, array, block)
    in if cfile /= file then (cfile, carray, cblock) else
    let (lfile, larray, lblock) = getlist (file, array, block)
    in if lfile /= file then (lfile, larray, lblock) else (file,array,block)

parsebody :: (String, [String], Document) -> (String, [String], Document)
parsebody (file, newfile, newdoc) = (file, newfile, newdoc)

getbodybalise :: (String, [String], Document) -> (String, [String], Document)
getbodybalise (file, array, newfile) =
   case runParser (parseString "<body>\n") file of
      Just(_,rest)->(removeLeadingSpaces rest,push "</body>\n" array,newfile)
      Nothing -> error "missing body"

parsingxml :: (String, Document) -> Document
parsingxml (file, newfile) = head
  where
    (nfile, array, ndoc) = getroot (file, newfile)
    (headless, head) = getheader (nfile, ndoc)
    (bodyfile, newarray, bodydoc) = getbodybalise (headless, array, newfile)

parsexml :: String -> Document -> Document
parsexml file newfile = parsingxml (file, newfile)


--trace headless $