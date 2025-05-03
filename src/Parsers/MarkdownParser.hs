{-
-- EPITECH PROJECT, 2025
-- B-FUN-400-TLS-4-1-mypandoc-hugo.poggetti
-- File description:
-- Markdown Parser
-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Parsers.MarkdownParser (parsemd) where
import Ast.Document
import Data.List (isPrefixOf)
import Lib
import Utils (splitOne)
import Debug.Trace (trace)

data MdBlock
  = MdParagraph String
  | MdCodeBlock String
  | MdList [String]
  | MdHeader Int String [MdBlock]
  deriving (Show, Eq)

groupMarkdown :: String -> Int -> [MdBlock]
groupMarkdown "" _ = []
groupMarkdown content _ =
  let ls = lines content
      (blocks, _)= parseLines ls 0
  in blocks

parseLines :: [String] -> Int -> ([MdBlock], [String])
parseLines [] _ = ([], [])
parseLines (l:ls) level
  | "```" `isPrefixOf` l = createcodeb (l:ls) level
  | "-" `isPrefixOf` l = createlist (l:ls) level
  | null l = parseLines ls level
  | "#" `isPrefixOf` l = createheader (l:ls) level
  | otherwise =
  let (blocks, remaining) = parseLines (dropWhile null ls) level
  in (MdParagraph l : blocks, remaining)

createheader :: [String] -> Int -> ([MdBlock], [String])
createheader (l:ls) level =
  let clevel = length (takeWhile ( =='#') l)
      (header, rest) = mdSection (l:ls) level
      (blocks, remaining) = parseLines rest level
  in if clevel /= 0 && clevel <= level then (blocks, l:ls) else (header : blocks, remaining)

createcodeb :: [String] -> Int -> ([MdBlock], [String])
createcodeb (_:ls) level =
   let (codeLines, rest) = break (isPrefixOf "```") ls
       rest' = drop 1 rest
       (blocks, remaining) = parseLines rest' level
  in (MdCodeBlock (unlines codeLines) : blocks, remaining)

createlist :: [String] -> Int -> ([MdBlock], [String])
createlist (l:ls) level =
  let (items,rest)=span (\line->"-"`isPrefixOf`line) (l:ls)
      (blocks, remaining) = parseLines rest level
  in (MdList items : blocks, remaining)

ismissingsection :: [String] ->Int -> Int -> (MdBlock, [String])
ismissingsection (l:ls) level clevel=
  let (nextsect, file) = mdSection (l:ls) (level+1)
      (inblock, nfile) = parseLines ls clevel
      line = splitOne ' ' l
  in if clevel > (level+1) then (MdHeader (level+1) "" [nextsect], file) else
    (MdHeader clevel (last line) inblock, nfile)

mdSection :: [String] -> Int -> (MdBlock, [String])
mdSection (l:ls) level =
  let clevel = length (takeWhile ( =='#') l)
      (inblock, file)= parseLines ls clevel
  in if clevel == 0 then (MdHeader (level+1) "" inblock, file) else
    ismissingsection (l:ls) level clevel

mdBlockToBlock :: MdBlock -> Block
mdBlockToBlock (MdCodeBlock txt) = CodeBlock txt
mdBlockToBlock (MdList items) =
  BulletList (map (\item -> [Para [Str (drop 2 item)]]) items)
mdBlockToBlock (MdHeader level text blocks) =
  let title = maybe [] (\t -> [Str t]) (Just text)
  in Section level title (map mdBlockToBlock blocks)


mdBlockToBlock (MdParagraph lines) =
  Para (parseMarkdownInlines lines)

parseMarkdownBlocks :: String -> [Block]
parseMarkdownBlocks content =
  let grouped = groupMarkdown content 0
  in map mdBlockToBlock grouped

parsemd :: String -> Document -> Maybe Document
parsemd input _ =
  let (meta, body) = parseMetadataAndBody input
      blocks = parseMarkdownBlocks body
  in Just (Document meta blocks)

parseMetadataAndBody :: String -> (Meta, String)
parseMetadataAndBody input =
  case runParser (parseString "---\n") input of
    Just (_, rest) -> parseMetadataWithBody rest
    Nothing        -> (defaultMeta, input)

parseMetadataWithBody :: String -> (Meta, String)
parseMetadataWithBody rest =
  let (meta, body) = parseMetadata rest
  in case runParser (parseString "---\n") body of
      Just (_, finalBody) -> (meta, finalBody)
      Nothing             -> (meta, body)

parseMetadata :: String -> (Meta, String)
parseMetadata input =
  let (title, rest1)  = parseField "title:" input
      (author, rest2) = parseField "author:" rest1
      (date, rest3)   = parseField "date:" rest2
  in (Meta
        { metaTitle = [Str title]
        , metaAuthors = [[Str author]]
        , metaDate = [Str date]
        }, rest3)

parseField :: String -> String -> (String, String)
parseField key input =
  case runParser (parseString key) input of
    Just (_, rest) ->
      let (valueLine:remaining) = lines rest
      in (valueLine, unlines remaining)
    Nothing -> ("", input)

defaultMeta :: Meta
defaultMeta = Meta [] [] []

parseMarkdownInlines :: String -> [Inline]
parseMarkdownInlines "" = []
parseMarkdownInlines s = case s of
    ('*':'*':rest) -> parseDelimited "**" Strong rest
    ('*':rest)     -> parseDelimited "*" Emph rest
    ('`':rest)     -> parseCodeInline rest
    ('!':'[':rest) -> parseImage rest
    ('[':rest)     -> parseLink rest
    _ -> let (text, rest) = span (`notElem` "*`![") s
      in Str text : parseMarkdownInlines rest

parseDelimited :: String -> ([Inline] -> Inline) -> String -> [Inline]
parseDelimited delim constructor input =
  let (inner, rest') = breakOn delim input
  in case rest' of
       Just rest -> constructor (parseMarkdownInlines inner) :
        parseMarkdownInlines (drop (length delim) rest)
       Nothing   -> 
        let (text, rest) = span (`notElem` "*`![") input
        in Str (delim++text): parseMarkdownInlines rest

parseCodeInline :: String -> [Inline]
parseCodeInline input =
  let (code, rest') = breakOn "`" input
  in case rest' of
       Just rest -> Code code : parseMarkdownInlines (drop 1 rest)
       Nothing   -> 
        let (text, rest) = span (`notElem` "*`![") input
        in Str ("`"++text): parseMarkdownInlines rest

parseLink :: String -> [Inline]
parseLink input =
  let (txt, rest1) = breakOn "]" input
  in case rest1 of
    Just urlRest -> let (url, rest2) = breakOn ")" urlRest
      in case rest2 of
          Just r ->Link (parseMarkdownInlines txt) (drop 2 url, "") :
            parseMarkdownInlines (drop 1 r)
          Nothing -> [Str ("[" ++ input)]
    Nothing ->  let (text, rest) = span (`notElem` "*`![") input
      in Str ("["++text): parseMarkdownInlines rest

parseImage :: String -> [Inline]
parseImage input =
  let (alt, rest1) = breakOn "]" input
  in case rest1 of
    Just urlRest -> let (url, rest2) = breakOn ")" urlRest
      in case rest2 of
        Just r -> Image (parseMarkdownInlines alt) (drop 2 url, "") :
          parseMarkdownInlines (drop 1 r)
        Nothing ->[Str ("![" ++ input)]
    Nothing -> let (text, rest) = span (`notElem` "*`![") input
      in Str ("!["++text): parseMarkdownInlines rest

removeParens :: String -> String
removeParens = filter (`notElem` "()")

breakOn :: String -> String -> (String, Maybe String)
breakOn delim s =
  let (x, y) = breakOn' s
  in (x, y)
  where
    breakOn' [] = ([], Nothing)
    breakOn' str@(c:cs)
      | delim `isPrefixOf` str = ([], Just str)
      | otherwise =
          let (a, b) = breakOn' cs
          in (c:a, b)