{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-hugo.poggetti
-- File description:
-- Main
-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Parsers.XmlParser (parsexml) where

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
         Nothing -> ("", Nothing)
    Nothing -> ("", Nothing)

parseauthor :: String -> (String, Maybe String)
parseauthor file =
   case runParser (parseString "<author>") file of
    Just (_, rest) ->
      let authParts = splitOne '<' rest
          auth = head authParts
      in case runParser (parseString "</author>\n") ("<"++last authParts) of
         Just (_, rest) -> (removeLeadingSpaces rest, Just auth)
         Nothing -> ("", Nothing)
    Nothing -> ("", Nothing)

getDateAndAuth :: String -> (String, Maybe String, Maybe String)
getDateAndAuth file =
   case runParser (parseString ">\n") file of
      Just (_, rest) ->
         let (afile, auth) = parseauthor (removeLeadingSpaces rest)
             (dfile, date) = parsedate afile
         in case runParser (parseString "</header>\n")
         (removeLeadingSpaces dfile) of
            Just (_, rest) -> (rest, auth, date)
            Nothing -> ("", Nothing, Nothing)
      Nothing -> ("", Nothing, Nothing)

parseheader :: String -> (String, Maybe String, Maybe String, Maybe String)
parseheader file =
  case runParser (parseString "<header title=\"") file of
    Just (_, rest) ->
      let titleParts = splitOne '\"' rest
          title= head titleParts
      in  
         let (headerless, auth, date) = getDateAndAuth (last titleParts)
         in ((removeLeadingSpaces headerless), Just title, auth, date)
    Nothing -> ("", Nothing, Nothing, Nothing)

getheader :: (String, Document) -> (String, Maybe Document)
getheader (file, newfile) = 
   let (headerless, title, auth, date) = parseheader file
       newMeta = Meta {
         metaTitle = [Str (fromJust title)],
         metaAuthors = [[Str (fromJust auth)]],
         metaDate = [Str (fromJust date)]
      }
       ndoc = if title == Nothing || auth == Nothing || date == Nothing then
         Nothing else Just (Document newMeta
         (case newfile of Document _ blocks->blocks))
   in (headerless, ndoc)

getroot :: (String, Document) -> (String, [String], Maybe Document)
getroot (file, newfile) =
   case runParser (parseString "<document>\n") file of
      Just (result, rest) -> (removeLeadingSpaces rest, [result],Just newfile)
      Nothing -> ("", [], Nothing)

-- a
-- a
-- a
-- a
-- a
-- a
-- a
-- a
-- a

getbold :: (String, [String], [Inline]) -> (String, [String], Maybe [Inline])
getbold (file , array, inline) = (file , array, Just inline)

getitalic :: (String, [String], [Inline]) -> (String, [String], Maybe [Inline])
getitalic (file , array, inline) = (file , array, Just inline)

getcode :: (String, [String], [Inline]) -> (String, [String], Maybe [Inline])
getcode (file , array, inline) = (file , array, Just inline)

getlink :: (String, [String], [Inline]) -> (String, [String], Maybe [Inline])
getlink (file , array, inline) = (file , array, Just inline)

getimage :: (String, [String], [Inline]) -> (String, [String], Maybe [Inline])
getimage (file , array, inline) = (file , array, Just inline)

-- a
-- a
-- a
-- a

getcodeblock :: (String, [String], [Block]) -> (String, [String], Maybe [Block])
getcodeblock (file , array, block) = (file , array, Just block)

getlist :: (String, [String], [Block]) -> (String, [String], Maybe [Block])
getlist (file , array, block) = (file , array, Just block)

getparagraph :: (String, [String], [Block]) -> (String, [String], Maybe [Block])
getparagraph (file , array, block) = (file , array, Just block)

getsection :: (String, [String], [Block]) -> (String, [String], Maybe [Block])
getsection (file , array, block) = 
   case runParser (parseString "<section title=\"") file of
    Just (_, rest) ->
      let sectionParts = splitOne '>' rest
          section = head sectionParts
      in (removeLeadingSpaces rest, push "</section>" array, Just block)
    Nothing -> (file , array, Nothing)

isinline :: (String, [String], [Inline]) -> (String, [String], Maybe [Inline])
isinline (file, array, inline)=let(bfil,barray,bin)=getbold(file,array,inline)
   in if bfil /= file then (bfil, barray, bin) else
   let (efile, earray, eblock) = getitalic (file, array, inline)
   in if efile /= file then (efile, earray, eblock) else
   let (cfile, carray, cblock) = getcode (file, array, inline)
   in if cfile /= file then (cfile, carray, cblock) else
   let (lfile, larray, lblock) = getlink (file, array, inline)
   in if lfile /= file then (lfile, larray, lblock) else
   let (ifile, iaray, iblock) = getimage (file, array, inline)
   in if ifile/=file then(ifile,iaray, iblock)else(file,array,Just inline)

isaclosing :: (String, [String], [Block]) -> (String, [String], Maybe [Block])
isaclosing (file, array, block) =
  case runParser (parseString "</") file of
    Just (_, rest1) ->
      case array of
        (toclose:restArray) ->
          case runParser (parseString toclose) rest1 of
            Just (_, rest2)->(removeLeadingSpaces rest2,restArray,Just block)
            Nothing -> (file, array, Nothing)
        [] -> (file, array, Nothing)
    Nothing -> (file, array, Nothing)

isablock :: (String, [String], [Block]) -> (String, [String], Maybe [Block])
isablock (file, array, block) =
    let (pfile, parray, pblock) = getparagraph (file, array, block)
    in if pfile /= file then (pfile, parray, pblock) else
    let (sfile, sarray, sblock) = getsection (file, array, block)
    in if sfile /= file then (sfile, sarray, sblock) else
    let (cfile, carray, cblock) = getcodeblock (file, array, block)
    in if cfile /= file then (cfile, carray, cblock) else
    let (lfile, laray, lblock) = getlist (file, array, block)
    in if lfile/=file then(lfile,laray, lblock)else(file,array,Just block)

parsebody :: (String, [String], Document) -> (String, [String], Maybe Document)
parsebody (file, newfile, newdoc) = (file, newfile, Just newdoc)

getbodybalise :: (String, [String], Document) -> (String, [String], Maybe Document)
getbodybalise (file, array, newfile) =
   case runParser (parseString "<body>\n") file of
      Just(_,rest)->(removeLeadingSpaces rest,push "</body>\n" array,
         Just newfile)
      Nothing -> ("", [], Nothing)

parsingxml :: (String, Document) -> Maybe Document
parsingxml (file, newfile) =
    let (nfile, array, ndoc) = getroot (file, newfile)
        (headless, head) = if ndoc == Nothing then (file, Nothing) else
         getheader (nfile, fromJust ndoc)
        (bodyfile, newarray, bodydoc) = if head == Nothing then ("", [], Nothing)
         else getbodybalise(headless, array, fromJust head)
    in Just newfile

parsexml :: String -> Document -> Maybe Document
parsexml file newfile = parsingxml (file, newfile)


--trace headless $
