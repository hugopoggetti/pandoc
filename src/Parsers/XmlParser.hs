{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-hugo.poggetti
-- File description:
-- Main
-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Parsers.XmlParser (parsexml) where

import Ast.Document (Document(..), Meta (..), newdoc, Inline (..), Block (..))
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

-- date and author meta

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

-- header parsing and title recuperation

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

-- matadata

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

-- root file

getroot :: (String, Document) -> (String, [String], Maybe Document)
getroot (file, newfile) =
   case runParser (parseString "<document>\n") file of
      Just (result, rest) -> (removeLeadingSpaces rest, [result],Just newfile)
      Nothing -> ("", [], Nothing)

-- bold inline

getbold :: (String, [String], [Inline]) -> (String, [String], Maybe [Inline])
getbold (file , array, inline) = (file , array, Just inline)

isbold :: (String, Int, [String], [Inline]) -> (String, [String], Maybe [Inline])
isbold (file, array, inline) =
   case runParser (parseString "<bold>") file of
    Just (_, rest) -> (rest, push "</paragraph>\n" array, Just inline)
    Nothing -> islink (file , array, inline)

-- italic inline

getitalic :: (String, [String], [Inline]) -> (String, [String], Maybe [Inline])
getitalic (file , array, inline) = (file , array, Just inline)

isitalic :: (String, Int, [String], [Inline]) -> (String, [String], Maybe [Inline])
isitalic (file, array, inline) =
   case runParser (parseString "<italic>") file of
    Just (_, rest) -> (rest, push "</paragraph>\n" array, Just inline)
    Nothing -> isbold (file , array, inline)
-- code text inline

getcode :: (String, [String], [Inline]) -> (String, [String], Maybe [Inline])
getcode (file , array, inline) = (file , array, Just inline)

iscode :: (String, Int, [String], [Inline]) -> (String, [String], Maybe [Inline])
iscode (file, array, inline) =
   case runParser (parseString "<code>") file of
    Just (_, rest) -> (rest, push "</paragraph>\n" array, Just inline)
    Nothing -> isitalic (file , array, inline)

-- THE SECOND STRING OF TARGET WILL BE EMPTY FOR THE MOMENT
-- link inline

getlink :: (String, [String], [Inline]) -> (String, [String], Maybe [Inline])
getlink (file , array, inline) = (file , array, Just inline)

islink :: (String, Int, [String], [Inline]) -> (String, [String], Maybe [Inline])
islink (file, array, inline)= 
   case runParser (parseString "<link url=\"") file of
    Just (_, rest) -> (rest, push "</paragraph>\n" array, Just inline)
    Nothing -> iscode (file , array, inline)

-- image inline

getimage :: (String, [String], [Inline]) -> (String, [String], Maybe [Inline])
getimage (file , array, inline) = 
   let newfile = splitOne "<" ++ file
       (nfile, narray, ninlines) = isimage (newfile, array, [])
   in 

isimage :: (String [String], [Inline]) -> (String, [String], Maybe [Inline])
isimage (file, array, inline)= 
   case runParser (parseString "<image url=\"") file of
    Just (_, rest) -> 
      let content = splitOne '>' rest
          ncontent = splitOne '<' (last content)
          parray = push "</image>" array
          (nfile,narray,image)=getimage(head ncontent,parray, [])
      in (nfile, narray, Just(inline ++[Image(fromJust image)
      (init(head content),"")]))
    Nothing -> islink (file , array, inline)

-- inlineless

gettext :: (String, String, [String], String, [Inline]) ->
   (String, [String], Maybe [Inline])
gettext (file, content, array, "</bold>", inline) = getbold(file,array,inline)
gettext (file, content, array,"</italic>",inline)=getitalic(file,array,inline)
gettext (file, content, array, "</code>", inline) = getcode(file,array,inline)
gettext (file, content, array, "</link>", inline) = getlink(file,array,inline)
gettext (file, content, array, "</image>", inline)=getimage(file,array,inline)
gettext (file, content, array, _, inline) = (file,array,Nothing)

isless :: (String, [String], [Inline]) -> (String, [String], Maybe [Inline])
isless (file, array, inline)= 
   case runParser (parseString "<") file of
    Just (_, _) -> isimage (file, array, inline)
    Nothing -> 
      let content = splitOne '<' file
          (elem, _) = pop array
      in if length array == 0 then (last content, array,
      Just(inline ++ [Str(head content)])) else
         gettext (last content, head content, array, fromJust elem, inline)

-- inlines

isinline :: (String, Int, [String], [Inline]) -> 
   (String, Int, [String], Maybe [Inline])
isinline (file, level, array, inline)= 
   let (nfile, narray, ninline) = isless (file, array, inline)
   in if file == "" || ninline == Nothing then 
   (file, level, array, Just inline) else
   isinline (nfile, level, narray, fromJust ninline)

-- closing balise

isaclosing :: (String, [String], [a]) -> (String, [String], Maybe [a])
isaclosing (file, array, block) =
  case runParser (parseString "</") file of
    Just (_, rest1) ->
      case array of
        (toclose:restArray) ->
          case runParser (parseString toclose) file of
            Just (_, rest2)->(removeLeadingSpaces rest2,restArray,Just block)
            Nothing -> (file, array, Nothing)
        [] -> (file, array, Nothing)
    Nothing -> (file, array, Just block)

-- paragraph

getparagraph :: (String, [String], [Block]) -> (String, [String], Maybe [Block])
getparagraph (file , array, block) =
   let (content, newfile) = splitstr file "</paragraph>\n"
       (_, _, array, inline) = if newfile == Nothing then
        ("", 0,array, Nothing) else isinline (content, 0, [], [])
   in if inline == Nothing then ("", array, Nothing) else
      (removeLeadingSpaces (fromJust newfile), array,
      Just(block ++ [Para (fromJust inline)]))

isaparagraph :: (String,[String],[Block]) -> (String, [String], Maybe [Block])
isaparagraph (file, array, block) =
   case runParser (parseString "<paragraph>\n") file of
    Just (_, rest) -> getparagraph (rest, array, block)
    Nothing -> isasection (file , array, block)

-- section

getsection :: (String, [String], [Block]) -> (String, [String], Maybe [Block])
getsection (file , array, block) =
   let content = splitOne '>' file
       ncontent = splitOne '\n' (last content)
       level = getiteration array 0 0
       newfile = removeLeadingSpaces (last ncontent)
       (nfile, narray, nblock) = isablock (newfile, array, [])
   in (nfile, narray,
   Just (block ++ [Section level [Str (head content)] (fromJust nblock)]))

isasection :: (String, [String], [Block]) -> (String, [String], Maybe [Block])
isasection (file, array, block) =
   case runParser (parseString "<section title=\"") file of
    Just (_, rest) -> getsection (rest, push "</section>\n" array, block)
    Nothing -> isacodeblock (file , array, block)

-- codeblock

getcodeblock :: (String, [String], [Block]) -> (String, [String], Maybe [Block])
getcodeblock (file , array, block) =
   let datatext = splitOne '<' file
   in case runParser (parseString "</paragraph>\n") ("<"++last datatext) of
      Just (_, r) ->
         case runParser (parseString "</codeblock>\n") (removeLeadingSpaces r)of
            Just(_, rest)->(removeLeadingSpaces rest,
               array,Just (CodeBlock (head datatext) : block))
            Nothing -> ("",array, Nothing)
      Nothing -> ("",array, Nothing)

isacodeblock :: (String, [String], [Block]) -> (String, [String], Maybe [Block])
isacodeblock (file, array, block) =
   case runParser (parseString "<codeblock>\n") file of
    Just (_, rest) ->
      case runParser (parseString "<paragraph>") (removeLeadingSpaces rest) of
         Just (_, rest1) -> (rest, array, Just block)
         Nothing -> isalist (file , array, block)
    Nothing -> isalist (file , array, block)

-- list

getlist :: (String, [String], Int, [[Block]]) -> (String, [String], Maybe [[Block]])
getlist (file , array, nb, block) =
   let newfile = removeLeadingSpaces file
       (nfile, narray, nblock) = isaparagraph (newfile, array, [])
   in if getiteration narray 0 0<nb then(nfile,narray,
   Just (block ++ [fromJust nblock])) else
   getlist (nfile, narray, nb, block ++ [fromJust nblock])

isalist :: (String, [String], [Block]) -> (String, [String], Maybe [Block])
isalist (file, array, block) =
   case runParser (parseString "<list>\n") file of
    Just (_, rest) ->
      let pfile = removeLeadingSpaces rest
          parray = push "</list>\n" array
          (nfile,narray,list)=getlist (pfile,parray,getiteration array 0 0,[])
      in (nfile, narray, Just(block ++ [BulletList (fromJust list)]))
    Nothing -> isaclosing (file , array, block)

-- block finder

isablock :: (String, [String], [Block]) -> (String, [String], Maybe [Block])
isablock (file, array, block) =
   let (nfile, narray, nblock) = isaparagraph (file, array, block)
   in if file == "" || nblock == Nothing then (file, array, Just block) else
   isablock (nfile, narray, fromJust nblock)

-- body parsing

parsebody :: (String, [String], Document) -> (String, [String], Maybe Document)
parsebody (file, newfile, newdoc) = (file, newfile, Just newdoc)

getbodybalise :: (String, [String], Document) -> (String, [String], Maybe Document)
getbodybalise (file, array, newfile) =
   case runParser (parseString "<body>\n") file of
      Just(_,rest)->(removeLeadingSpaces rest,push "</body>\n" array,
         Just newfile)
      Nothing -> ("", [], Nothing)

-- xml file parsing

parsingxml :: (String, Document) -> Maybe Document
parsingxml (file, newfile) =
    let (nfile, array, ndoc) = getroot (file, newfile)
        (headless, head) = if ndoc == Nothing then (file, Nothing) else
         getheader (nfile, fromJust ndoc)
        (bodyfile, newarray, bodydoc) = if head == Nothing then ("", [], Nothing)
         else getbodybalise (headless, array, fromJust head)
    in Just newfile

parsexml :: String -> Document -> Maybe Document
parsexml file newfile = parsingxml (file, newfile)


--trace headless $