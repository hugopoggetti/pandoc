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
import Data.Char (isSpace)
import Lib

parsemd :: String -> Document -> Maybe Document
parsemd input _ = parseMarkdownToDocument input

parseMarkdownToDocument :: String -> Maybe Document
parseMarkdownToDocument input = 
  let allLines = splitLines input
      (metaLines, bodyLines) = extractFrontMatter allLines
      meta = parseMeta metaLines
      blocks = parseBlocks bodyLines
  in Just (Document meta blocks)

splitLines :: String -> [String]
splitLines = lines

extractFrontMatter :: [String] -> ([String], [String])
extractFrontMatter allLines
  | length allLines >= 2 && head allLines == "---" =
      let (metaLines, rest) = break (== "---") (tail allLines)
      in if null rest then ([], allLines)
         else (metaLines, drop 1 rest)
  | otherwise = ([], allLines)

-- | Parse metadata from YAML front matter
parseMeta :: [String] -> Meta
parseMeta metaLines = Meta
  { metaTitle = parseMetaField "title:" metaLines,
    metaAuthors = [parseMetaField "author:" metaLines],
    metaDate = parseMetaField "date:" metaLines
  }

-- | Parse a specific metadata field
parseMetaField :: String -> [String] -> [Inline]
parseMetaField field lines =
  case filter (isPrefixOf field) lines of
    [] -> []
    (line:_) -> [Str (trimLeading (drop (length field) line))]

trimLeading :: String -> String
trimLeading = dropWhile isSpace

-- | Parse all blocks from document body
parseBlocks :: [String] -> [Block]
parseBlocks [] = []
parseBlocks lines =
  let (block, remainingLines) = parseNextBlock lines
  in if null remainingLines
     then [block]
     else block : parseBlocks remainingLines

parseNextBlock :: [String] -> (Block, [String])
parseNextBlock [] = (Null, [])
parseNextBlock (line:rest)
  | all isSpace line = parseNextBlock rest
  | isPrefixOf "#" line = let depth = length $ takeWhile (=='#') line in
                          parseSection depth (drop (depth+1) line) rest
  | isPrefixOf "```" line = parseCodeBlock (drop 3 line) rest
  | isPrefixOf "- " line = parseBulletList (line:rest)
  | otherwise = parseParagraph (line:rest)

-- | Parse a section (header and its content)
parseSection :: Int -> String -> [String] -> (Block, [String])
parseSection level title rest =
  let titleInlines = parseInlines title
      (childBlocks, remaining) = collectSectionContent level rest
  in (Section level titleInlines childBlocks, remaining)

isNewSection :: Int -> String -> Bool
isNewSection level line =
  case countHeaderLevel line of
    Just l -> l <= level
    Nothing -> False

countHeaderLevel :: String -> Maybe Int
countHeaderLevel line =
  let hashes = takeWhile (== '#') line
      n = length hashes
  in if n > 0 && length line > n && line !! n == ' '
        then Just n
        else Nothing

collectSectionContent :: Int -> [String] -> ([Block], [String])
collectSectionContent _ [] = ([], [])
collectSectionContent level allLines@(line:rest)
  | isNewSection level line = ([], allLines)
  | all isSpace line = collectSectionContent level rest
  | otherwise =
      let (block, afterBlock) = parseNextBlock allLines
          (moreBlocks, finalRemaining) =
            collectSectionContent level afterBlock
      in (block : moreBlocks, finalRemaining)

-- | Parse a code block
parseCodeBlock :: String -> [String] -> (Block, [String])
parseCodeBlock _ lines =
  let (codeLines, rest) = break (isPrefixOf "```") lines
      code = unlines codeLines
  in (CodeBlock code, if null rest then [] else tail rest)

-- | Parse a bullet list
parseBulletList :: [String] -> (Block, [String])
parseBulletList lines =
  let (items, rest) = parseListItems lines
  in (BulletList items, rest)

parseOrderedList :: [String] -> (Block, [String])
parseOrderedList lines =
  let (items, rest) = parseListItems lines
      attributes = (1, Decimal, Period)
  in (OrderedList attributes items, rest)

addItem :: [[Block]] -> [String] -> [[Block]]
addItem acc [] = acc
addItem acc itemLines = parseItemContent itemLines : acc

isListItemStart :: String -> Bool
isListItemStart l =
  let trimmed = trimLeading l
  in isPrefixOf "- " trimmed || isOrderedListItem trimmed

isBlockBreak :: String -> Bool
isBlockBreak l =
  isPrefixOf "#" l || isPrefixOf "```" l

isOrderedListItem :: String -> Bool
isOrderedListItem line =
  any (\prefix -> isPrefixOf prefix (trimLeading line)) 
      ["1. ", "2. ", "3. ", "4. ", "5. ", "6. ", "7. ", "8. ", "9. ", "0. ", 
       "1) ", "2) ", "3) ", "4) ", "5) ", "6) ", "7) ", "8) ", "9) ", "0) "]

parseListItems :: [String] -> ([[Block]], [String])
parseListItems [] = ([], [])
parseListItems lines = go lines [] []
  where
    go [] accItems currItem = (reverse $ addItem accItems currItem, [])
    go (l:ls) accItems currItem
      | all isSpace l = go ls accItems currItem
      | isListItemStart l = let updatedItems = addItem accItems currItem
          in go ls updatedItems [l]
      | isBlockBreak l =
          (reverse $ addItem accItems currItem, l:ls)
      | otherwise = go ls accItems (currItem ++ [l])
    
parseItemContent :: [String] -> [Block]
parseItemContent [] = []
parseItemContent (firstLine:rest) = 
    let content = dropListMarker firstLine ++ 
            if null rest then "" else "\n" ++ unlines rest
    in [Para (parseInlines content)]

dropListMarker :: String -> String
dropListMarker line
  | isPrefixOf "- " (trimLeading line) = drop 2 (trimLeading line)
  | isOrderedListItem line = 
      let trimmed = trimLeading line
          indexOfDot = indexOf '.' trimmed
          indexOfParen = indexOf ')' trimmed
          index = if indexOfDot > 0 then indexOfDot else indexOfParen
      in if index > 0 then trimLeading (drop (index + 1) trimmed) else trimmed
  | otherwise = line

indexOf :: Char -> String -> Int
indexOf c s = case dropWhile (/= c) s of
  [] -> -1
  (_:_) -> length (takeWhile (/= c) s)

-- | Parse a paragraph
parseParagraph :: [String] -> (Block, [String])
parseParagraph [] = (Null, [])
parseParagraph lines =
  let (paraLines, rest) = break isParagraphBreak lines
      content = unwords paraLines
  in (Para (parseInlines content), rest)

isParagraphBreak :: String -> Bool
isParagraphBreak "" = True
isParagraphBreak line =
  isPrefixOf "#" line || 
  isPrefixOf "```" line || 
  isPrefixOf "- " (trimLeading line) || 
  isOrderedListItem line

spanUntil :: String -> String -> Maybe (String, String)
spanUntil delim input =
  case breakOn delim input of
    (before, Just after) -> Just (before, after)
    (_, Nothing) -> Nothing

breakOn :: String -> String -> (String, Maybe String)
breakOn _ [] = ([], Nothing)
breakOn delim s@(x:xs)
  | delim `isPrefixOf` s = ("", Just (drop (length delim) s))
  | otherwise =
      let (before, found) = breakOn delim xs
      in (x:before, found)
-- | Parse inline elements
parseInlines :: String -> [Inline]
parseInlines "" = []
parseInlines ('*':'*':rest) =
  case spanUntil "**" rest of
    Just (content, remaining) ->
      Strong (parseInlines content) : parseInlines remaining
    Nothing -> Str "*" : parseInlines ('*':rest)
parseInlines ('*':rest) =
  case span (/= '*') rest of
    (content, '*' : remaining) ->
      Emph (parseInlines content) : parseInlines remaining
    _ -> Str "*" : parseInlines rest
parseInlines ('`':rest) =
  let (content, remaining) = break (== '`') rest
  in if null remaining
     then Str ('`' : content) : parseInlines ""
     else Code content : parseInlines (tail remaining)
parseInlines ('[':rest) =
  case parseLinkOrImage rest of
    Just (inlines, target, remaining) -> 
        Link inlines target : parseInlines remaining
    Nothing -> Str "[" : parseInlines rest
parseInlines ('!':'[':rest) =
  case parseLinkOrImage rest of
    Just (inlines, target, remaining) -> 
        Image inlines target : parseInlines remaining
    Nothing -> Str "![" : parseInlines rest
parseInlines (c:rest) =
  let (text, remaining) = span (not . isSpecialChar) (c:rest)
  in Str text : parseInlines remaining

isSpecialChar :: Char -> Bool
isSpecialChar c = c `elem` "*`[]!()"

parseLinkOrImage :: String -> Maybe ([Inline], Target, String)
parseLinkOrImage str = 
  let (textContent, rest1) = break (== ']') str
  in case () of
       _ | null rest1 || length rest1 < 2 -> Nothing
         | head (tail rest1) /= '(' -> Nothing
         | otherwise -> 
             let (url, rest2) = break (== ')') (drop 2 rest1)
             in if null rest2
                then Nothing
                else Just (parseInlines textContent, (url, ""), tail rest2)
